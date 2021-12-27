#!/usr/bin/env bash
set -eo pipefail

k3d cluster delete

# Not sure about this *required* node-filter "server:0". What if I had multiple nodes? I don't want traefik, though.
k3d cluster create \
   --registry-create registry \
   --api-port host.docker.internal:42042 \
   -p "8081:80@loadbalancer" \
   --k3s-arg '--no-deploy=traefik@server:0'

# https://github.com/prometheus-community/helm-charts
# https://github.com/grafana/helm-charts/tree/main/charts/grafana
# Delete CRDs after uninstalling: https://github.com/prometheus-community/helm-charts/issues/557
# Set root_url in conjunction with our ingress
helm repo add prometheus-community https://prometheus-community.github.io/helm-charts
helm install prometheus prometheus-community/kube-prometheus-stack \
   --set 'grafana.grafana\.ini'.server.root_url=http://host.docker.internal:3000/grafana/

echo "Grafana <user,pw>: \
   $(kubectl get secret prometheus-grafana --output json | jq -r '.data."admin-user"' | base64 -d) \
   $(kubectl get secret prometheus-grafana --output json | jq -r '.data."admin-password"' | base64 -d)"


# https://github.com/elastic/helm-charts
helm repo add elastic https://helm.elastic.co

# Pod anti-affinity settings mandate one pod per node (so you'd need three nodes for the default
# setting of 3 pods). This relaxes that requirement. Though, I think k3d would be fine with "3" nodes.
helm install elasticsearch elastic/elasticsearch --set antiAffinity=soft

helm install logstash elastic/logstash

# Unset resources. Otherwise, pod requirements aren't able to be met (1 cpu). Not sure _why_ they can't be met,
# but this fixes the problem.
helm install kibana elastic/kibana --set resources=null -f <(echo '---
kibanaConfig:
   kibana.yml: |
      server:
         basePath: /kibana
         rewriteBasePath: false
')

helm install filebeat elastic/filebeat

# Remember, we didn't install traefik. That's so we could use nginx instead.
# I had issues with Traefik, namely:
# k3s uses an old version.
# Their documentation appeared to be WRONG for whatever version k3s was using.
# I spent hours trying to get a rewrite URL rule to remove the "matched" part of the url, eg:
# If pattern is "/foo"
# and matched URL is "/foo/bar"
# I wanted to redirect to "/bar" on the service specified in the match rule. Could NOT get it to work.
helm repo add nginx-stable https://helm.nginx.com/stable
helm repo update
helm install my-release nginx-stable/nginx-ingress


./dashboard.sh

./ingress.sh
