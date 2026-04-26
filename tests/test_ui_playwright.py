import pytest


pytestmark = pytest.mark.ui


def test_homepage_search_form_is_usable(page, live_server):
    page.goto(f"{live_server}/")

    page.get_by_label("Enter a word or phrase you are looking for:").fill("indeed")
    page.get_by_label("Who said it?").fill("TEAL'C")
    page.get_by_role("button", name="Search").click()

    page.wait_for_url("**/search**")
    assert "speech=indeed" in page.url
    assert "person=TEAL%27C" in page.url
    assert page.locator(".alert").first.text_content()


def test_transcript_index_links_to_transcript(page, live_server):
    page.goto(f"{live_server}/transcripts")

    page.get_by_text("Children Of The Gods", exact=False).first.click()
    page.wait_for_url("**/transcripts/sg1/1.1")
    page.get_by_role("heading", name="SG1 Episode 1.01").wait_for()


def test_search_results_link_to_line_anchor(page, live_server):
    page.goto(f"{live_server}/search?speech=indeed&series=sg1")

    first_result = page.locator(".panel-heading a").first
    first_result.wait_for()
    href = first_result.get_attribute("href")

    assert href is not None
    assert href.startswith("/transcripts/sg1/")
    assert "#L" in href


def test_random_page_keeps_same_seed_output(page, live_server):
    url = f"{live_server}/random/ui-seed"
    page.goto(url)
    first = page.locator("pre").text_content()

    page.goto(url)
    second = page.locator("pre").text_content()

    assert "Randomly Generated Episode" in page.locator("body").text_content()
    assert first == second


def test_mobile_homepage_does_not_overflow(page, live_server):
    page.set_viewport_size({"width": 390, "height": 844})
    page.goto(f"{live_server}/")

    scroll_width = page.evaluate("document.documentElement.scrollWidth")
    client_width = page.evaluate("document.documentElement.clientWidth")

    assert scroll_width <= client_width
    page.get_by_role("button", name="Search").is_visible()
