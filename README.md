# TL;DR

## Pull and run

```
docker run -itp 3020:3020 likelion/dklare
```

## Build from source and run

```
git clone https://github.com/likelion/dklare.git
cd dklare
docker build -t dklare .
docker run -itp 3020:3020 dklare
```

# Configuration

## Volume for knowledge files

```
-v /path/to/folder:/opt/dklare/knowledge
```

## Volume for RDF store

```
-v /path/to/folder:/opt/dklare/RDF-store
```
