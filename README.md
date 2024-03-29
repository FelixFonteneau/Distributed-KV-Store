# ID2203 Project Kompics Scala

This project is an implementation of a key-value distributed store based on [Kompics-Scala](https://kompics.github.io/kompics-scala/). 
The architecture follows a leader based sequence paxos to satisfy the linearizability property. For more information and an overview on my work, 
see my [report](report.pdf).

## Overview

The project is split into 3 sub parts:

- A common library shared between servers and clients, containing mostly messages and similar shared types
- A server library that manages bootstrapping and membership
- A client library with a simple CLI to interact with a cluster of servers

The bootstrapping procedure for the servers, requires one server to be marked as a bootstrap server, which the other servers (bootstrap clients) check in with, before the system starts up. The bootstrap server also assigns initial partitions.

## Getting Started

`git clone` (your fork of) the repository to your local machine and `cd` into that folder.

Make sure you have [sbt](https://www.scala-sbt.org/) installed.

---
### Building

Start sbt with

```bash
sbt
```

In the sbt REPL build the project with

```bash
compile
```

You can run the test suite (which includes simulations) with

```bash
test
```

Before running the project you need to create assembly files for the server and client:

```bash
server/assembly
client/assembly
```

---
### Running

#### Bootstrap Server Node
To run a bootstrap server node execute:

```bash
java -jar server/target/scala-2.13/server.jar -p 45678
```

This will start the bootstrap server on localhost:45678.

#### Normal Server Node
After you started a bootstrap server on `<bsip>:<bsport>`, again from the `server` directory execute:

```bash
java -jar server/target/scala-2.13/server.jar -p 45679 -s <bsip>:<bsport>
```
This will start the bootstrap server on localhost:45679, and ask it to connect to the bootstrap server at `<bsip>:<bsport>`.
Make sure you start every node on a different port if they are all running directly on the local machine.

By default you need 3 nodes (including the bootstrap server), before the system will actually generate a lookup table and allow you to interact with it.
The number can be changed in the configuration file (cf. [Kompics docs](http://kompics.github.io/current/tutorial/networking/basic/basic.html#cleanup-config-files-classmatchers-and-assembly) for background on Kompics configurations).

#### Clients
To start a client (after the cluster is properly running) execute:

```bash
java -jar client/target/scala-2.13/client.jar -p 56787 -s <bsip>:<bsport>
```

Again, make sure not to double allocate ports on the same machine.

The client will attempt to contact the bootstrap server and give you a small command promt if successful. Type `help` to see the available commands.

---
### Setting up cluster using Script

The above instructions are how to manually set up a cluster. There are some scripts provided to simplify 
the creation of a cluster. You may modify the files how you want.

Start a cluster with 3 servers (Remember that this number may need to change if you adjust the Bootstrap threshold):

```bash
./cluster_setup.sh 3
```

Connect with client:

```bash
./client.sh
```

---
### Executing tests
This project contains advanced test scenarios using the [Kompics Simulation framework](https://kompics.github.io/kompics-scala/tutorial/simulation/).

Tests contain:
- Simple tests on the validity of the requests commands (GET, PUT, CAS)
- Scenario of test
- An advanced test framework on _Linearizability_ property. This contains algorithms to test linearizability (based on this [reference](https://www.cs.ox.ac.uk/people/gavin.lowe/LinearizabiltyTesting/paper.pdf)), 
and a simulation with multiple scenarios involving random requests of multiple clients requesting different nodes of the system and with nodes dying.

To run tests:
```bash
sbt test
```
