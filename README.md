# JoesFlowHPC

The JoesFlowHPC branches contain the wrapper package, `JoesFlowHPC`, for automating various manual steps required to run large data sets in JoesFlow, to manage data from multiple users, and to interact with a high performance cluster. The config file located at `inst/config.yml` contains the necessary information to connect to Skyline at NIAID. This can be modified or additional profiles can be added for other HPC systems.

## Installation

## Configuration

## connection to Skyline

1. Create an RSA key

```bash
ssh-keygen -t rsa -b 4096
```

2. Copy the public key to the server

```bash
ssh-copy-id -i ~/.ssh/id_rsa.pub username@server
```

