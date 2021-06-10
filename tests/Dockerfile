FROM erlang:alpine

# Build
RUN mkdir /buildroot
WORKDIR /buildroot
COPY src src
COPY rebar.config rebar.config
RUN rebar3 as prod release

# Installation
WORKDIR /
RUN mkdir /etc/MeowMeow/
COPY tests/config/routes.conf /etc/MeowMeow/routes.conf
COPY tests/config/meow.conf /etc/MeowMeow/meow.conf
RUN cp -r /buildroot/_build/prod/rel/MeowMeow /MeowMeow
COPY tests/test.sh /usr/bin/TestMeowMeow
RUN chmod +x /usr/bin/TestMeowMeow
COPY tests/www /var/www

# Tests
WORKDIR /
COPY tests/simple_test tests
COPY tests/vsn.txt /tests/vsn.txt
RUN mkdir /testroot
WORKDIR /testroot
RUN apk add ruby ruby-libs ruby-json ruby-bigdecimal ruby-bundler git
RUN git clone https://github.com/Andrewerr/SimpleTest
WORKDIR /testroot/SimpleTest/
RUN bundle install

# Do tests on run
WORKDIR /
ENTRYPOINT ["/usr/bin/TestMeowMeow"]
