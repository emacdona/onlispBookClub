#!/usr/bin/env bash
set -eo pipefail

k3d cluster delete

# Not sure about this *required* node-filter "server:0". What if I had multiple nodes? I don't want traefik, though.
# Keycloak, f*ck off. I spent DAYS trying to get keycloak to work with forwarding host port 8081 to LoadBalancer port 80.
# THERE IS NO WAY TO TELL KEYCLOAK TO ADD THE F*CKING :8080 AS THE PORT IN THE REDIRECT AND NAVIGATION URLS IT GENERATES.
# So now we're listening on 80. Makes the urls look nicer, I guess.
#
# On baremetal Linux, host.docker.internal resolves to 172.17.0.1, as desired. However, in WSL 2, it appears to resolve to
# 192.168.1.111 -- which is... maybe my windows host's IP address? (and not the vm running all the docker containers?). Anyway
# It doesn't work for what I want here. Note that this was AFTER I gave up on docker desktop and installed plain ol' dockerd
# per here: https://dev.to/bowmanjd/install-docker-on-windows-wsl-without-docker-desktop-34m9 (Mostly... I editorialized).
# The reason I gave up on docker desktop was because the connection between the container and the X Server on the host was
# UNUSABLY slow (at least that's my theory). Now that I know there are two "host" ips in question, it may be worth switching back
# To Docker Desktop and trying both of them to see if one is better than the other.

# 32022: gitlab ssh port. Not load balancer, it's a nodeport
# https://k3d.io/v5.0.0/usage/exposing_services/#2-via-nodeport

# Ports 80,443 => nginx ingress
# Ports 8080,8443 => istio ingress
k3d cluster create \
   --registry-create registry \
   --api-port 172.17.0.1:42042 \
   -p "80:80@loadbalancer" \
   -p "443:443@loadbalancer" \
   -p "8080:8080@loadbalancer" \
   -p "8443:8443@loadbalancer" \
   -p "32022:32022@server:0" \
   --k3s-arg '--no-deploy=traefik@server:0'

# Uncomment this when messing testing ansible scripts. This line allows you to use this script for creating the k3d cluster only:
# exit 0

# https://github.com/prometheus-community/helm-charts
# https://github.com/grafana/helm-charts/tree/main/charts/grafana
# Delete CRDs after uninstalling: https://github.com/prometheus-community/helm-charts/issues/557
# Set root_url in conjunction with our ingress
helm repo add prometheus-community https://prometheus-community.github.io/helm-charts
helm install prometheus prometheus-community/kube-prometheus-stack

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
helm install kibana elastic/kibana --set resources=null

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
helm install nginx-ingress nginx-stable/nginx-ingress

# Cassandra and MongoDB (two separate edges of the CAP theorem)
# https://github.com/bitnami/charts/tree/master/bitnami/cassandra
# https://github.com/bitnami/charts/tree/master/bitnami/mongodb
helm repo add bitnami https://charts.bitnami.com/bitnami
helm install cassandra bitnami/cassandra
helm install my-release bitnami/mongodb

# Jenkins
# https://github.com/jenkinsci/helm-charts/blob/main/charts/jenkins/README.md
helm repo add jenkins https://charts.jenkins.io
helm repo update
helm install jenkins jenkins/jenkins \
   --set controller.ingress.enabled=true \
   --set controller.ingress.hostName=jenkins.test \
   --set controller.ingress.ingressClassName=nginx

./dashboard.sh

./ingress.sh

./keycloak/kc

# Gitlab
# https://gist.github.com/nirbhabbarat/8fe32ccaaacc782272c9f49a753e97b4
# https://docs.gitlab.com/charts/development/minikube/
helm repo add gitlab https://charts.gitlab.io/
helm repo update

helm upgrade --install gitlab gitlab/gitlab \
   --timeout 600s \
   --set global.hosts.gitlab.hostnameOverride=gitlab.test \
   --set global.hosts.externalIP=172.17.0.1 \
   -f https://gitlab.com/gitlab-org/charts/gitlab/raw/master/examples/values-minikube.yaml
