# Iglu Server with Docker Compose

This [docker compose](https://docs.docker.com/compose/) setup is intended for development only.  The `docker-compose.yml` file has configuration for

- An iglu server container
- A postgres database container

Launch the stack with:

```bash
docker-compose up -d

```

### API keys

Iglu server must be configured with a super api key, which has permissions to add other api keys, and to read/write any schema.
By default, the docker-compose file uses the following api key:

```
5fb4713d-73ad-4163-93a9-2b82f0177c5b
```

You can override this API key by setting the `IGLU_MASTER_API_KEY` environment variable:

```bash
IGLU_MASTER_API_KEY=35cba2e2-7f16-422d-b780-e43b84270fcc docker-compose up -d

```

Or alternatively edit the `superApiKey` field in [the sample hocon file](../config/config.minimal.hocon).

### Initialise the database

This is a one-off command that you run to initialise the database tables when you launch the stack for the first time.

```bash
docker-compose run iglu-server setup --config /snowplow/config/config.hocon
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
