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
* Use some software without leaving an *install* footprint on our host machine
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
|     DEBIAN_FRONTEND=noninteractive apt-get install -y vim screen octave
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
| CMD ["octave", "--no-gui"]
```

-------------------------------------------------

-> # Let's Kick the Tires <-

-> (translation: "Demo Time") <-

