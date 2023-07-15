
# Notes

## ulimit (open files)
Opensearch requires a "Nubmer of open files" ulimit of greater than or equal to 65535

To achieve this, a couple of steps must be taken.

First, you must increase your limits in WSL. Include the following in `/etc/security/limits.conf`:
```
emacdona	soft	nofile		65536
emacdona	hard	nofile		65536
```

For whatever reason, however -- starting WSL does not seem to materialize these limits. So, secondly, after you start WSL, 
you must then su to yourself!
`su emacdona`

THEN these limits will be enforced.

Finally, start docker via:
```
sudo dockerd --default-ulimit nofile=65535:65535
```

These steps will ensure that docker starts all containers with the aforementioned "number of files" ulimit.