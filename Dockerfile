# 2020 - Build and export checker file
FROM golang:1.15-alpine AS aoc2020

WORKDIR /
COPY ./2020 .

RUN go build -o aoc .
RUN ["./aoc", "check"]

# Run checker
FROM python:3.9-alpine AS checker
RUN apk add build-base
RUN apk add libffi-dev
RUN apk add libressl-dev
RUN apk add musl-dev

RUN pip install ansible

# 2020 - Copy files to check
COPY --from=aoc2020 /results /2020_results
COPY --from=aoc2020 /expected_results /2020_expected_results

COPY checker.py /checker.py
ENTRYPOINT [ "python", "checker.py" ]
