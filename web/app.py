from flask import Flask, render_template, request
import psycopg2, psycopg2.extras, os, time

app = Flask(__name__)

conn = psycopg2.connect(user="transcripts",
                        dbname="postgres",
                        password=os.environ["DB_PASS"],
                        host=os.environ["DB_HOST"],
                        port=5432)

print("Connected to database")

gcur = conn.cursor(cursor_factory=psycopg2.extras.DictCursor)

# try to load the episode list, it will work eventually, when the DB
# gets the data
gcur.execute("SELECT DISTINCT ON (series, season_number, episode_number) * FROM transcripts ORDER BY series, season_number, episode_number")

conn.commit()
entries = gcur.fetchall()

@app.route("/")
def index():
    return render_template("index.html")

@app.route("/about")
def about():
    return render_template("about.html")

@app.route("/advanced")
def advanced():
    return render_template("advanced.html")

@app.route("/random")
def random():
    return render_template("random.html")

def speech(r):
    return "{}: {}".format(r['person'], r['speech'])

def get_contexts(row):
    cur = conn.cursor(cursor_factory=psycopg2.extras.DictCursor)
    query_string = "SELECT * from transcripts WHERE id <= %(id)s + 2 AND id >= %(id)s - 2"
    cur.execute(query_string, {'id':row['id']})
    results = cur.fetchall()
    conn.commit()

    def same_scene(r):
        return all([r['series'] == row['series'],
                    r['season_number'] == row['season_number'],
                    r['episode_number'] == row['episode_number'],
                    r['scene_number'] == row['scene_number']])

    before = [speech(r) for r in results if r['line_number'] < row['line_number'] and same_scene(r)]
    after = [speech(r) for r in results if r['line_number'] > row['line_number'] and same_scene(r)]
    return (before, after)

def make_result(row):
    result = {}
    result['url'] = '/transcripts/{}/{}.{}'.format(row['series'],
                                                   row['season_number'],
                                                   row['episode_number'])
    result['episode'] = "{}: Season {}, Episode {}: {}".format(row['series'].upper(),
                                                               row['season_number'],
                                                               row['episode_number'],
                                                               row['episode_title'])
    result['place'] = row['place']
    result['context_before'], result['context_after'] = get_contexts(row)
    result['match'] = speech(row)
    return result

@app.route("/search")
def search():
    search_cols = ["speech", "person", "present", "place", "episode_title"]
    search_strings = []
    andor = " OR " if request.args.get("andor") == "or" else " AND "

    for col in search_cols:
        if request.args.get(col) not in [None, ""]:
            search_strings.append(f'{col} ILIKE %({col})s')

    # Series is a special case since it can have multiple values and
    # the user *always* wants to OR on series
    seriess = ["sg1", "atl", "tos", "ent", "ds9", "tng", "voy"]
    series_strings = ["series = '{}'".format(series) for series in seriess if series in request.args.getlist("series")]

    series_string = " OR ".join(series_strings)
    search_string = andor.join(search_strings)

    search_query = '''SELECT * FROM transcripts {} {} {} {} LIMIT 1000
    '''.format("WHERE" if len(search_string) > 0 or len(series_string) > 0 else "",
               search_string,
               "AND" if len(series_string) > 0 and len(search_string) > 0 else "",
               f'({series_string})' if len(series_string) > 0 else "")

    print(search_query)

    cur = conn.cursor(cursor_factory=psycopg2.extras.DictCursor)
    results = []
    error = False

    try:
        cur.execute(search_query,
                    { col:"%{}%".format(request.args.get(col)) for col in search_cols if request.args.get(col) not in [None, ""] })
        results = cur.fetchall()
        conn.commit()
    except psycopg2.DatabaseError as e:
        print(e)
        conn.rollback()
        error = True

    results = [make_result(row) for row in results]
    return render_template("results.html",
                           results=results,
                           advanced=False,
                           error=error)

@app.route("/transcripts")
def transcripts():
    return render_template("transcript_index.html",
                           entries=entries)

@app.route("/transcripts/<series>/<raw_epcode>")
def transcript(series, raw_epcode):
    epcode = raw_epcode
    if series in ["sg1", "atl"]:
        seas, ep = raw_epcode.split(".")
        epcode = '{}.{:02d}'.format(seas, int(ep))
    parsed = ""
    with open(f'pretty/{series}/{epcode}', 'r') as f:
        parsed = f.read()
    return render_template("transcript.html",
                           episode="{} Episode {}".format(series.upper(), epcode),
                           parsed_transcript=parsed)

if __name__ == "__main__":
    app.run(port=8000, host="0.0.0.0")
