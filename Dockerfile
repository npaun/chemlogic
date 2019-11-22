FROM swipl:7.7.25 as builder
WORKDIR /app
COPY . /app
RUN apt update && apt install -y \
 	make \ 
  && rm -rf /var/lib/apt/lists/* \ 
  && make web

FROM swipl:7.7.25
WORKDIR /app
COPY --from=builder /app/bin .
EXPOSE 8000
USER 10001
CMD ["swipl","-t","keep_running","-x","chemweb"]
