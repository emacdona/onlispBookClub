#!/usr/bin/env bash
set -eo pipefail

# https://kubernetes.io/docs/tasks/access-application-cluster/web-ui-dashboard/

kubectl \
   apply \
   -f https://raw.githubusercontent.com/kubernetes/dashboard/v2.4.0/aio/deploy/recommended.yaml

echo 'apiVersion: v1
kind: ServiceAccount
metadata:
  name: admin-user
  namespace: kubernetes-dashboard
' | kubectl create -f -


echo 'apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRoleBinding
metadata:
  name: admin-user
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: ClusterRole
  name: cluster-admin
subjects:
  - kind: ServiceAccount
    name: admin-user
    namespace: kubernetes-dashboard
' | kubectl create -f -


echo
echo "Your k8s dashboard token:"
kubectl \
   -n kubernetes-dashboard \
   get secret \
   $(kubectl -n kubernetes-dashboard get sa/admin-user -o jsonpath="{.secrets[0].name}") \
   -o go-template="{{.data.token | base64decode}}"
echo
echo
