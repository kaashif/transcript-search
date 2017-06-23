from flask import Flask, render_template, request
import os

app = Flask(__name__)
transcript_lines = {
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
    os.chdir(os.path.dirname(os.path.abspath(__file__)))
    for dire in ["sg1", "atl"]:
        for fname in os.listdir(dire):
            [season, episode] = fname.split(".")
            with open(os.path.join(os.getcwd(), dire, fname), "r", encoding="latin-1") as transcript:
                transcript_lines[dire][fname] = transcript.readlines()
                    
    os.chdir(olddir)

def do_search(query):
    results = []
    for (dire, series) in [("sg1", "SG1"), ("atl", "Atlantis")]:
        for fname in transcript_lines[dire].keys():
            [season, episode] = fname.split(".")
            lines = transcript_lines[dire][fname]
            for i in range(len(lines)):
                if query in lines[i]:
                    results.append({
                        "episode": "Stargate {}: Season {}, Episode {}".format(series, season, episode),
                        "context_before": "".join(lines[max(0,i-5):i]),
                        "context_after": "".join(lines[min(i+1,len(lines)):i+6]),
                        "match": lines[i],
                        "url":"/transcripts/{}/{}.{}".format(dire, season, episode)
                    })
                    
    return results
