import pytest

import app as transcript_app


def row(
    *,
    row_id=1,
    series="sg1",
    season_number=1,
    episode_number=1,
    episode_title="Children Of The Gods Part 1",
    scene_number=1,
    line_number=1,
    person="CARTER",
    present="CARTER O'NEILL",
    place="GATE ROOM",
    speech="Indeed, this is a test line.",
):
    return {
        "id": row_id,
        "series": series,
        "season_number": season_number,
        "episode_number": episode_number,
        "episode_title": episode_title,
        "scene_number": scene_number,
        "line_number": line_number,
        "person": person,
        "present": present,
        "place": place,
        "speech": speech,
    }


def test_int_parses_empty_values_as_zero():
    assert transcript_app._int("") == 0
    assert transcript_app._int("12") == 12


def test_speech_formats_person_and_line():
    assert transcript_app.speech(row(person="TEAL'C", speech="Indeed.")) == "TEAL'C: Indeed."


def test_same_scene_compares_episode_and_scene_identity():
    base = row()

    assert transcript_app.same_scene(base, row(row_id=2, line_number=2))
    assert not transcript_app.same_scene(base, row(scene_number=2))
    assert not transcript_app.same_scene(base, row(series="atl"))


@pytest.mark.parametrize(
    ("query", "expected"),
    [
        ("", {"sg1", "atl", "tos", "ent", "ds9", "tng", "voy"}),
        ("?series=sg1&series=voy", {"sg1", "voy"}),
    ],
)
def test_selected_series_uses_all_series_by_default(flask_app, query, expected):
    with flask_app.test_request_context("/search" + query):
        assert transcript_app.selected_series() == expected


def test_matches_uses_and_semantics(flask_app):
    candidate = row(speech="The gate is active.", person="CARTER", series="sg1")

    with flask_app.test_request_context("/search?series=sg1"):
        assert transcript_app.matches(
            candidate,
            ["speech", "person"],
            {"speech": "gate", "person": "carter"},
            "and",
        )
        assert not transcript_app.matches(
            candidate,
            ["speech", "person"],
            {"speech": "gate", "person": "oneill"},
            "and",
        )


def test_matches_uses_or_semantics(flask_app):
    candidate = row(speech="The gate is active.", person="CARTER", series="sg1")

    with flask_app.test_request_context("/search?series=sg1"):
        assert transcript_app.matches(
            candidate,
            ["speech", "person"],
            {"speech": "gate", "person": "oneill"},
            "or",
        )
        assert not transcript_app.matches(
            candidate,
            ["speech", "person"],
            {"speech": "wormhole", "person": "oneill"},
            "or",
        )


def test_matches_rejects_unselected_series(flask_app):
    candidate = row(series="voy")

    with flask_app.test_request_context("/search?series=sg1"):
        assert not transcript_app.matches(candidate, ["speech"], {"speech": "indeed"}, "and")


def test_make_result_builds_stargate_urls():
    result = transcript_app.make_result(row(line_number=42))

    assert result["url"] == "/transcripts/sg1/1.1#L42"
    assert result["episode"] == "SG1: Season 1, Episode 1: Children Of The Gods Part 1"
    assert result["place"] == "GATE ROOM"
    assert result["match"] == "CARTER: Indeed, this is a test line."


def test_make_result_builds_production_code_urls():
    result = transcript_app.make_result(
        row(
            series="tos",
            season_number=0,
            episode_number=1,
            episode_title="The Man Trap",
            line_number=7,
        )
    )

    assert result["url"] == "/transcripts/tos/1#L7"
    assert result["episode"] == "TOS: The Man Trap (1)"


def test_get_contexts_returns_nearby_lines_in_same_scene(monkeypatch):
    rows = [
        row(row_id=1, line_number=1, person="A", speech="before"),
        row(row_id=2, line_number=2, person="B", speech="match"),
        row(row_id=3, line_number=3, person="C", speech="after"),
        row(row_id=4, scene_number=2, line_number=4, person="D", speech="other scene"),
    ]
    monkeypatch.setattr(transcript_app, "transcript_rows", lambda: rows)

    before, after = transcript_app.get_contexts(rows[1])

    assert before == ["A: before"]
    assert after == ["C: after"]
