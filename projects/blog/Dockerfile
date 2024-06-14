FROM bitnami/ruby:3.1.0

COPY . .

RUN bundle config set --local path 'vendor/bundle'
RUN bundle install

USER root
ENTRYPOINT ["bundle", "exec", "jekyll", "serve", "--host", "0.0.0.0","--unpublished", "--trace"]