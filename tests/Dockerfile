FROM erlang:alpine

# Testing framework
WORKDIR /
COPY tests/simple_test tests
COPY tests/vsn.txt /tests/vsn.txt
RUN mkdir /testroot
WORKDIR /testroot
RUN apk add ruby ruby-libs ruby-json ruby-bigdecimal ruby-bundler git
RUN git clone https://github.com/Andrewerr/SimpleTest
WORKDIR /testroot/SimpleTest/
RUN bundle install

# Build
RUN mkdir /buildroot
WORKDIR /buildroot
COPY src src
COPY rebar.config rebar.config
RUN rebar3 as prod release

# Installation
WORKDIR /
RUN mkdir /etc/MeowMeow/
COPY tests/config/*.conf /etc/MeowMeow/
RUN cp -r /buildroot/_build/prod/rel/MeowMeow /MeowMeow
COPY tests/test.sh /usr/bin/TestMeowMeow
RUN chmod +x /usr/bin/TestMeowMeow
COPY tests/www /var/www

RUN dd if=/dev/urandom of=/var/www/FatCat bs=1024MB count=1 

# Do tests on run
WORKDIR /
ENTRYPOINT ["/usr/bin/TestMeowMeow"]
