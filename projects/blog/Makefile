
export BUILDKIT_PROGRESS := plain
export COMPOSE_MENU := 0

.PHONY: localdev
localdev:
	bundle config set --local path "vendor/bundle" \
	&& bundle install \
	&& bundle exec jekyll serve --host 0.0.0.0 --unpublished --trace

rollout:
	bundle config set --local path "vendor/bundle" \
	&& bundle install \
	&& ./sync.sh

.PHONY: clean
clean:
	docker compose down
