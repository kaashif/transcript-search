import app as transcript_app


def test_transcript_rows_load_real_dataset():
    rows = transcript_app.transcript_rows()

    assert len(rows) == 458892
    assert rows[0]["id"] == 1
    assert rows[-1]["id"] == len(rows)
    assert {
        "id",
        "series",
        "season_number",
        "episode_number",
        "episode_title",
        "scene_number",
        "line_number",
        "person",
        "present",
        "place",
        "speech",
    } <= set(rows[0])


def test_transcript_entries_are_unique_by_episode():
    entries = transcript_app.transcript_entries()
    keys = {
        (entry["series"], entry["season_number"], entry["episode_number"])
        for entry in entries
    }

    assert len(entries) == 990
    assert len(keys) == len(entries)
    assert ("sg1", 1, 1) in keys
    assert ("tos", 0, 1) in keys


def test_transcript_rows_are_sorted_for_stable_context_links():
    rows = transcript_app.transcript_rows()
    ordering = [
        (
            row["series"],
            row["season_number"],
            row["episode_number"],
            row["scene_number"],
            row["line_number"],
        )
        for row in rows[:5000]
    ]

    assert ordering == sorted(ordering)
