FROM erlang:alpine

# Prepare big file
RUN mkdir -p /var/www
RUN dd if=/dev/urandom of=/var/www/FatCat bs=1024MB count=1

# Testing framework
WORKDIR /
COPY tests/simple_test tests
COPY tests/vsn.txt /tests/vsn.txt
RUN mkdir /testroot
WORKDIR /testroot
RUN apk add ruby ruby-libs ruby-bigdecimal ruby-bundler ruby-dev build-base git
RUN git clone https://github.com/0xf104a/SimpleTest
WORKDIR /testroot/SimpleTest/
RUN bundle install

# Build
RUN mkdir /buildroot
WORKDIR /buildroot
COPY src src
COPY include include
COPY rebar.config rebar.config
RUN rebar3 as prod release

# Installation
WORKDIR /
RUN mkdir /etc/MeowMeow/
COPY tests/config/*.conf /etc/MeowMeow/
RUN cp -r /buildroot/_build/prod/rel/MeowMeow /MeowMeow
COPY tests/test.sh /usr/bin/TestMeowMeow
RUN chmod +x /usr/bin/TestMeowMeow
COPY tests/www /tmp/www
RUN mv /tmp/www/* /var/www/
RUN rm -rf /tmp/www

# Do tests on run
WORKDIR /
ENTRYPOINT ["/usr/bin/TestMeowMeow"]
