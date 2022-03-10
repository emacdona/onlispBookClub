%title: Running Programs with Docker on your Linux Workstation 
%author: Ed MacDonald - Solution Street
%date: 2022-02-10

-> Running Programs with Docker on your Linux Workstation <-
=========

-------------------------------------------------
-> Who's heard of [Octave](https://www.gnu.org/software/octave/)? <-

<br>
-> Hopefully no one. It's more fun that way. <-

-------------------------------------------------

-> # Goals (What do we want?) <-

<br>
* Use some software ( *Octave* ) without leaving an *install* footprint on our host machine
<br>
* Use that software to create/modify files on the host
<br>
* End up being the owner of files we create on the host!
<br>
* GUI
<br>
   - Ed, are you serious?  
      <br> 
   - *We'll see...*

-------------------------------------------------

-> # The Dockerfile <-

```
| FROM ubuntu:20.04
| 
| RUN apt-get update && \ 
|     DEBIAN_FRONTEND=noninteractive \ 
|     apt-get install -y octave
|
| ARG UID
| ARG GID
| ARG USERNAME
| ARG USERGROUP
|
| RUN groupadd -g "${GID}" "${USERGROUP}"
| 
| RUN useradd \ 
|     -d "/home/${USERNAME}" \ 
|     -ms /bin/zsh \ 
|     -g "${USERGROUP}" \ 
|     -u $UID "${USERNAME}"
| 
| RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> \ 
|     /etc/sudoers
| 
| USER "${USERNAME}"
| 
| WORKDIR "/home/${USERNAME}/workdir"
| 
| COPY myPlot.m myPlot.m
| 
| CMD ["octave", "--no-gui", "--quiet"]
```

-------------------------------------------------

-> # Let's Kick the Tires <-

-> (translation: "Demo Time") <-

-------------------------------------------------

-> # Thanks for watching! <-

-------------------------------------------------

-> # We've still got time left?! <-

<br>
-> ## Let's keep going... <-

<br>
* Why Linux and not Mac?
<br>
   * Docker on Mac is virtualized.
<br>
   * Bind mounts to the "host" (OS X) filesystem have to traverse a VM (Linux) layer.
<br>
   * No idea how this happens (NFS?), but...
<br>
   * This makes bind mounts VERY slow.
<br>
* Okay, why Linux and not Windoze?
<br>
   * I can't stand Windows, but...
<br>
   * Docker may(?) run natively in WSL.
<br>
   * I *think* WSL is, itself, native (ie: NOT emulated Linux).
<br>
   * This *could* mean bind mounts will work really well on Window$!
<br>
   * Definitely tell me if you know this to be the case.
