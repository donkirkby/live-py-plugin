## Prebuilding dev container images
Each subfolder contains the instructions for prebuilding one of the dev
container images. To rebuild an image, install the [dev container CLI], then
build.

    npm install --location=global @devcontainers/cli
    cd /path/to/live-py-plugin/.devcontainer-prebuild/full
    devcontainer build --workspace-folder . --push false --image-name donkirkby/live-py-plugin-devcontainer-full:v1.0

If you want to share your changes, create an account on docker hub, run
`docker login`, then replace `donkirkby` with your account and change to
`--push true`.

[dev container CLI]: https://code.visualstudio.com/docs/devcontainers/devcontainer-cli
