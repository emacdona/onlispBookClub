Okay, there are a few things to note here.

I'm lazy, so I want to use an existing helm chart. It gets me up and running quickly so I can start messing around. Someday, when it fails to give me the flexibility that I (someday) require, I can rethink that decision. Maybe make my own helm chart. Maybe fork this one. Who knows?

I also want to be able to play around with the keycloak SCIM plugin. And that puts me in a pickle. The SCIM plugin has a compatibility matrix. You need a certain version of the plugin with a certain version of Keycloak. The version of Keycloak installed by the chart I'm using sometimes gets ahead of the latest version of Keycloak supported by the plugin.

helm search repo keycloak --versions

to the rescue! Sort of. You can compare the output of that to the table on this page:
https://github.com/Captain-P-Goldfish/scim-for-keycloak

To figure out the latest versions of the chart and plugin that are compatible.

However, the plugin needs to be "installed", which means we need to build our own docker image into which we've copied it's deployable artifact (where JBoss can find it). This means we need to decide on a base image. Ideally, of course, we would use exactly the same JBoss image that is deployed by the version of the chart we have chosen.

However, I don't see version tags in the git repo for the chart, so I'm thinking that maybe the only way to figure it out is to deploy the chart with the default image, look at the Pod manifest it creates, and then use that as our base image. Hopefully I find a better way than that.

'helm pull' looks promising...

BOOM! See 'chartVersionFinder' in this directory. It shows how to find the chart version and base image we are looking for.
