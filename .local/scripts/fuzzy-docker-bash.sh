#!/usr/bin/env bash

# COMPOSE_FILE=/var/lib/rpst-api-docker/docker-compose.yml:/var/lib/rpst-api-docker/docker-compose.rpst-v2-dev.yml docker compose  run --rm $(COMPOSE_FILE=/var/lib/rpst-api-docker/docker-compose.yml:/var/lib/rpst-api-docker/docker-compose.rpst-v2-dev.yml docker compose ps | tail -n +2 | awk '{print $4}' | fzf) bash
COMPOSE_FILE=~/dev/rpst-api/docker-compose.yml:~/dev/rpst-api/docker-compose.rpst-v2-dev.yml docker compose  run --rm $(COMPOSE_FILE=~/dev/rpst-api/docker-compose.yml:~/dev/rpst-api/docker-compose.rpst-v2-dev.yml docker compose ps | tail -n +2 | awk '{print $4}' | fzf) bash
