#!/bin/sh

set -e

sudo -u postgres psql -c "DROP DATABASE IF EXISTS testdb;"
sudo -u postgres psql -c "CREATE DATABASE testdb;"
