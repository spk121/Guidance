This may someday be a graphical debugger for GNU Guile.

At the moment, it is garbage code.

Do not build.
Do not view.
Do not feel the need to tell me it is broken.

## Docker container

If the CI/CD system is working, it is creating the `spk121/guidance` Docker container.
You can pull down the Docker container with
```
docker pull spk121/guidance
```
or
```
podman pull docker.io/spk121/guidance
```

Running a Docker container that contains a GUI requires an invocation like the following
```
docker run -v /tmp/.X11-unix:/tmp/.X11-unix \
    -e DISPLAY=$DISPLAY -h $HOSTNAME spk121/guidance /app/bin/guidance

```
