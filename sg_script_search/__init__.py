from flask import Flask, render_template, request
import os
import json

app = Flask(__name__)
transcript_lines = {
    'sg1':{},
    'atl':{}
}
transcript_json = {
    'sg1':{},
    'atl':{}
}

@app.route("/")
def index():
    return render_template("index.html")

@app.route("/about")
def about():
    return render_template("about.html")

@app.route("/search", methods=["POST"])
def search():
    query = request.form.get("query", "default")
    print(query)
    results = do_search(query)
    return render_template("results.html", results=results)

@app.route("/style.css")
def style():
    return app.send_static_file("style.css")

@app.route("/transcripts")
def transcript_index():
    return render_template("transcript_index.html")

@app.route("/transcripts/<series>/<episode>")
def transcript(series, episode):
    [season, ep] = episode.split(".")
    return render_template("transcript.html", text="".join(transcript_lines[series][episode]), episode="{} Season {} Episode {}".format(series.upper(), season, ep))

def main():
    cache_transcripts()
    app.run()

def cache_transcripts():
    olddir = os.getcwd()
    os.chdir(os.path.join(os.path.dirname(os.path.abspath(__file__)), "raw"))
    for dire in ["sg1", "atl"]:
        for fname in os.listdir(dire):
            [season, episode] = fname.split(".")
            with open(os.path.join(os.getcwd(), dire, fname), "r", encoding="latin-1") as transcript:
                transcript_lines[dire][fname] = transcript.readlines()
    os.chdir(os.path.join(os.path.dirname(os.path.abspath(__file__)), "json"))
    for dire in ["sg1", "atl"]:
        for fname in os.listdir(dire):
            [season, episode, _] = fname.split(".")
            with open(os.path.join(os.getcwd(), dire, fname), "r", encoding="latin-1") as transcript:
                transcript_json[dire]["{}.{}".format(season, episode)] = json.load(transcript)
    os.chdir(olddir)

def strline(lines, i, j):
    toprint = lines[i:j]
    return ["{}: {}".format(c, l) for [c, l] in toprint]

def do_search(query):
    results = []
    for (dire, series) in [("sg1", "SG1"), ("atl", "Atlantis")]:
        for fname in transcript_json[dire].keys():
            [season, episode] = fname.split(".")
            for scene in transcript_json[dire][fname]:
                lines = scene["speech"]
                for i in range(len(lines)):
                    if query in lines[i][1]:
                        results.append({
                            "episode": "Stargate {}: Season {}, Episode {}".format(series, season, episode),
                            "context_before": strline(lines, max(0,i-2), i),
                            "context_after": strline(lines, min(i+1,len(lines)), i+3),
                            "match": "{}: {}".format(lines[i][0], lines[i][1]),
                            "url":"/transcripts/{}/{}.{}".format(dire, season, episode),
                            "place": scene["place"]
                        })
                    
    return results
