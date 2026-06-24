require "testapi/http.rb"

module Tests
  def self.test_env_def
    r = TestHTTP::get("http://localhost/")
    r.headers["X-Environment-Present"] == "1"
  end

  def self.test_env_value
    r = TestHTTP::get("http://localhost/")
    r.headers["X-Environment"] == "docker"
  end
end