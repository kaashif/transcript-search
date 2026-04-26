import threading
import sys
from pathlib import Path

import pytest
from werkzeug.serving import make_server

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

import app as transcript_app


@pytest.fixture
def flask_app():
    transcript_app.app.config.update(TESTING=True)
    return transcript_app.app


@pytest.fixture
def client(flask_app):
    return flask_app.test_client()


@pytest.fixture(scope="session")
def live_server():
    server = make_server("127.0.0.1", 0, transcript_app.app)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield f"http://127.0.0.1:{server.server_port}"
    finally:
        server.shutdown()
        thread.join(timeout=5)
