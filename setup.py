#!/usr/bin/env python3
from setuptools import setup

setup(
    name = "sg_script_search",
    version = "0.0.1",
    author = "kaashif",
    description = ("Stargate script search web app"),
    license = "BSD",
    keywords = "web search app",
    url = "http://github.com/kaashif/sg-script-search",
    packages=['sg_script_search'],
    classifiers=[
        "Development Status :: 3 - Alpha",
        "License :: OSI Approved :: BSD License",
        "Programming Language :: Python :: 3.4",
        "Operating System :: POSIX"
    ],
    scripts = [
        "scripts/sg_script_search"
    ],
    install_requires = [
        "Flask"
    ],
    zip_safe=False,
    include_package_data=True,
)
