import re


def test_index_page_contains_search_form(client):
    response = client.get("/")

    assert response.status_code == 200
    assert b"Transcript Search" in response.data
    assert b'name="speech"' in response.data
    assert b'name="series"' in response.data


def test_about_and_advanced_pages_render(client):
    assert client.get("/about").status_code == 200
    advanced = client.get("/advanced")

    assert advanced.status_code == 200
    assert b"season_number" in advanced.data
    assert b"episode_title" in advanced.data


def test_transcript_index_lists_known_series_and_episode(client):
    response = client.get("/transcripts")

    assert response.status_code == 200
    assert b"SG1" in response.data
    assert b"Children Of The Gods" in response.data
    assert b"TOS" in response.data


def test_stargate_transcript_route_pads_episode_number(client):
    response = client.get("/transcripts/sg1/1.1")

    assert response.status_code == 200
    assert b"SG1 Episode 1.01" in response.data
    assert b"Children Of The Gods" in response.data


def test_trek_transcript_route_uses_production_code(client):
    response = client.get("/transcripts/tos/1")

    assert response.status_code == 200
    assert b"TOS Episode 1" in response.data


def test_unknown_transcripts_return_404(client):
    assert client.get("/transcripts/bad/1").status_code == 404
    assert client.get("/transcripts/sg1/not-an-episode").status_code == 404
    assert client.get("/transcripts/sg1/99.99").status_code == 404


def test_search_filters_by_speech_and_series(client):
    response = client.get("/search?speech=indeed&series=sg1")

    assert response.status_code == 200
    assert b"Found <strong>" in response.data
    assert b"TEAL" in response.data or b"Indeed" in response.data
    assert b"SG1:" in response.data


def test_search_without_filters_returns_first_thousand_results(client):
    response = client.get("/search")

    assert response.status_code == 200
    assert b"Found &gt;1000 results" in response.data


def test_search_or_mode_matches_any_parameter(client):
    response = client.get("/search?speech=window&person=TEAL%27C&andor=or&series=sg1")

    assert response.status_code == 200
    assert b"results" in response.data


def test_search_can_return_no_results(client):
    response = client.get("/search?speech=zzzzzzzz-not-present&series=sg1")

    assert response.status_code == 200
    assert b"No results found" in response.data


def test_random_without_seed_redirects_to_seeded_url(client):
    response = client.get("/random/")

    assert response.status_code == 302
    assert re.match(r"/random/[a-f0-9]{32}$", response.headers["Location"])


def test_seeded_random_page_is_deterministic(client):
    first = client.get("/random/repeatable-seed")
    second = client.get("/random/repeatable-seed")

    assert first.status_code == 200
    assert first.data == second.data
    assert b"Randomly Generated Episode" in first.data
