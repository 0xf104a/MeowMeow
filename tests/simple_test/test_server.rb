require "testapi/http.rb"

$testroot = ""

if ENV["TESTROOT"].nil? then
   puts "TESTS WARNING: TESTROOT is not set. Please use `export TESTROOT=<path to directory with vsn.txt>` to set it."
else
   $testroot = ENV["TESTROOT"]
end

module Tests
  def self.test_server_header
      r = TestHTTP::get("http://localhost")
      vsn = "MeowMeow/"+File.read(File.join($testroot,"vsn.txt"))
      r.headers["Server"] == vsn.strip
  end
end
