
export BUILDKIT_PROGRESS := plain
export COMPOSE_MENU := 0

.PHONY: localdev
localdev:
	docker compose up --build

rollout:
	docker compose -f docker-compose-rollout.yaml up --build

.PHONY: clean
clean:
	docker compose down
