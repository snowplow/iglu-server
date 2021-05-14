# Iglu Server with Docker Compose

This [docker compose](https://docs.docker.com/compose/) setup is intended for development only.  The `docker-compose.yml` file has configuration for

- An iglu server container
- A postgres database container

Launch the stack with:

```bash
docker-compose up -d
```

### Initialise the database

This is a one-off command that you run to initialise the database tables when you launch the stack for the first time.

```bash
docker-compose run iglu-server setup --config /snowplow/config/config.hocon
```

### Set the API key

The easiest way to set an API key is to `exec` into the postgres container:

```bash
docker-compose exec postgres psql -U postgres igludb -c "$(cat << 'EOH'
  INSERT INTO iglu_permissions(apikey, vendor, wildcard, schema_action, key_action)
  VALUES ('5fb4713d-73ad-4163-93a9-2b82f0177c5b', '', 'TRUE', 'CREATE_VENDOR'::schema_action, '{"CREATE", "DELETE"}'::key_action[])
EOH
)"
```

### Using your iglu server

```bash
# Create a schema

curl -XPOST localhost:8080/api/schemas \
  -H "apikey: 5fb4713d-73ad-4163-93a9-2b82f0177c5b" \
  -d '
        {
          "$schema": "http://iglucentral.com/schemas/com.snowplowanalytics.self-desc/schema/jsonschema/1-0-0#",
          "self": {
            "vendor": "com.acme",
            "name": "myschema",
            "format": "jsonschema",
            "version": "1-0-0"
          },
          "type": "object"
        }'

# Now list schema you added

curl localhost:8080/api/schemas -H "apikey: 5fb4713d-73ad-4163-93a9-2b82f0177c5b"
```
