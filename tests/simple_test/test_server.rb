require "testapi/http.rb"
require "testapi/file.rb"
require "digest"

$testroot = ""

if ENV["TESTROOT"].nil? then
   puts "TESTS WARNING: TESTROOT is not set. Please use `export TESTROOT=<path to directory with vsn.txt>` to set it."
else
   $testroot = ENV["TESTROOT"]
end

module Tests
  def self.get_vsn
      vsn = File.read(File.join($testroot, "vsn.txt"))
      "MeowMeow/"+vsn.strip
  end
  def self.test_server_header
      r = TestHTTP::get("http://localhost")
      vsn = self.get_vsn
      r.headers["Server"] == vsn.strip
  end
  def self.test_big_file
      r = TestHTTP::get("http://localhost/FatCat")
      print "#{r.duration}s..."
      Digest::MD5.hexdigest(r.data) == FileAPI::md5("/var/www/FatCat") and r.duration > 0.020
  end
end
