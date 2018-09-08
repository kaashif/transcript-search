from flask import Flask, render_template, request
import psycopg2, psycopg2.extras, os

app = Flask(__name__)

conn = psycopg2.connect(user="transcripts",
                        dbname="postgres",
                        password=os.environ["DB_PASS"],
                        host="localhost")

@app.route("/")
def index():
    return render_template("index.html")

@app.route("/about")
def about():
    return render_template("about.html")

@app.route("/advanced")
def advanced():
    return "There is no advanced search yet, go back."

@app.route("/random")
def random():
    return render_template("random.html")

def speech(r):
    return "{}: {}".format(r['person'], r['speech'])

def get_contexts(row):
    cur = conn.cursor(cursor_factory=psycopg2.extras.DictCursor)
    query_string = "SELECT * FROM transcripts WHERE series=%s AND episode_number=%s AND season_number=%s AND scene_number=%s AND abs(line_number - %s) <= 2"
    cur.execute(query_string,
                [row['series'], row['episode_number'], row['season_number'], row['scene_number'], row['line_number']])
    results = cur.fetchall()
    conn.commit()
    before = [speech(r) for r in results if r['line_number'] < row['line_number']]
    after = [speech(r) for r in results if r['line_number'] > row['line_number']]
    return (before, after)

def make_result(row):
    result = {}
    result['url'] = ''
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
    return render_template("transcripts.html")
