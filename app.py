import csv
import glob
import os
import random
import uuid
from functools import lru_cache
from pathlib import Path

from flask import Flask, redirect, render_template, request


BASE_DIR = Path(__file__).resolve().parent
SERIES = ["sg1", "atl", "tos", "ent", "ds9", "tng", "voy"]
TREK_RANDOM_SERIES = ["ent", "tos", "tng", "ds9", "voy"]

app = Flask(
    __name__,
    template_folder=str(BASE_DIR / "web" / "templates"),
    static_folder=str(BASE_DIR / "web" / "static"),
    static_url_path="/static",
)


def _int(value):
    return int(value) if value else 0


@lru_cache(maxsize=1)
def transcript_rows():
    rows = []
    with open(BASE_DIR / "data" / "transcripts.tsv", newline="", encoding="latin-1") as f:
        reader = csv.reader(f, delimiter="\t")
        for row in reader:
            if len(row) != 10:
                continue
            record = {
                "series": row[0],
                "season_number": _int(row[1]),
                "episode_number": _int(row[2]),
                "episode_title": row[3],
                "scene_number": _int(row[4]),
                "line_number": _int(row[5]),
                "person": row[6],
                "present": row[7],
                "place": row[8],
                "speech": row[9],
            }
            rows.append(record)

    rows.sort(
        key=lambda r: (
            r["series"],
            r["season_number"],
            r["episode_number"],
            r["scene_number"],
            r["line_number"],
        )
    )
    for idx, row in enumerate(rows, start=1):
        row["id"] = idx
    return rows


@lru_cache(maxsize=1)
def transcript_entries():
    seen = set()
    entries = []
    for row in transcript_rows():
        key = (row["series"], row["season_number"], row["episode_number"])
        if key in seen:
            continue
        seen.add(key)
        entries.append(row)
    return entries


def speech(row):
    return "{}: {}".format(row["person"], row["speech"])


def same_scene(left, right):
    return all(
        [
            left["series"] == right["series"],
            left["season_number"] == right["season_number"],
            left["episode_number"] == right["episode_number"],
            left["scene_number"] == right["scene_number"],
        ]
    )


def get_contexts(row):
    rows = transcript_rows()
    start = max(row["id"] - 3, 0)
    end = min(row["id"] + 2, len(rows))
    nearby = rows[start:end]
    before = [
        speech(candidate)
        for candidate in nearby
        if candidate["line_number"] < row["line_number"] and same_scene(candidate, row)
    ]
    after = [
        speech(candidate)
        for candidate in nearby
        if candidate["line_number"] > row["line_number"] and same_scene(candidate, row)
    ]
    return before, after


def make_result(row):
    result = {}
    if row["season_number"] != 0:
        result["url"] = "/transcripts/{}/{}.{}#L{}".format(
            row["series"],
            row["season_number"],
            row["episode_number"],
            row["line_number"],
        )
        result["episode"] = "{}: Season {}, Episode {}: {}".format(
            row["series"].upper(),
            row["season_number"],
            row["episode_number"],
            row["episode_title"],
        )
    else:
        result["url"] = "/transcripts/{}/{}#L{}".format(
            row["series"], row["episode_number"], row["line_number"]
        )
        result["episode"] = "{}: {} ({})".format(
            row["series"].upper(), row["episode_title"], row["episode_number"]
        )
    result["place"] = row["place"]
    result["context_before"], result["context_after"] = get_contexts(row)
    result["match"] = speech(row)
    return result


def selected_series():
    requested = request.args.getlist("series")
    return set(requested) if requested else set(SERIES)


def matches(row, search_cols, terms, andor):
    checks = [
        terms[col].casefold() in row[col].casefold()
        for col in search_cols
        if terms.get(col)
    ]
    if not checks:
        text_match = True
    elif andor == "or":
        text_match = any(checks)
    else:
        text_match = all(checks)
    return text_match and row["series"] in selected_series()


@app.route("/")
def index():
    return render_template("index.html")


@app.route("/about")
def about():
    return render_template("about.html")


@app.route("/advanced")
def advanced():
    return render_template("advanced.html")


@app.route("/search")
def search():
    search_cols = ["speech", "person", "present", "place", "episode_title"]
    terms = {
        col: request.args.get(col, "")
        for col in search_cols
        if request.args.get(col) not in [None, ""]
    }
    andor = "or" if request.args.get("andor") == "or" else "and"
    results = []
    for row in transcript_rows():
        if matches(row, search_cols, terms, andor):
            results.append(make_result(row))
            if len(results) == 1000:
                break
    return render_template("results.html", results=results, advanced=False, error=False)


@app.route("/transcripts")
def transcripts():
    return render_template("transcript_index.html", entries=transcript_entries())


@app.route("/transcripts/<series>/<raw_epcode>")
def transcript(series, raw_epcode):
    if series not in SERIES:
        return "Unknown series", 404
    epcode = raw_epcode
    if series in ["sg1", "atl"]:
        try:
            seas, ep = raw_epcode.split(".")
            epcode = "{}.{:02d}".format(seas, int(ep))
        except ValueError:
            return "Unknown episode", 404
    transcript_path = BASE_DIR / "web" / "pretty" / series / epcode
    if not transcript_path.is_file():
        return "Unknown episode", 404
    parsed = transcript_path.read_text(encoding="latin-1")
    return render_template(
        "transcript.html",
        episode="{} Episode {}".format(series.upper(), epcode),
        parsed_transcript=parsed,
    )


@app.route("/random/")
def random_noseed():
    digest = uuid.uuid4().hex
    return redirect(f"/random/{digest}", code=302)


@app.route("/random/<rand_seed>")
def random_seeded(rand_seed):
    my_random = random.Random(rand_seed)
    series = my_random.choice(TREK_RANDOM_SERIES)
    pattern = str(BASE_DIR / "web" / "sample_random" / series / "*.pretty")
    random_fname = my_random.choice(glob.glob(pattern))
    parsed = Path(random_fname).read_text(encoding="latin-1")
    return render_template(
        "random.html",
        episode=f"{series.upper()} Randomly Generated Episode",
        parsed_transcript=parsed,
    )


if __name__ == "__main__":
    port = int(os.environ.get("PORT", "8000"))
    app.run(port=port, host="0.0.0.0")
