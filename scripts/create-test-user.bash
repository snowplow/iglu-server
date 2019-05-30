#!/bin/sh
set -e
sudo -u postgres psql -c "create database testdb;"
exit 0
