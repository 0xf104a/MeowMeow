require "testapi/http.rb"
require "net/http"
require "uri"
require "base64"

module Tests
  def self.http_auth_get(path, headers = {})
    uri = URI("http://localhost#{path}")
    req = Net::HTTP::Get.new(uri)
    headers.each do |k, v|
      req[k] = v
    end
    Net::HTTP.start(uri.hostname, uri.port) do |http|
      http.request(req)
    end
  end

  # ==========================================
  # Authorize Context Rule - Bearer Token Tests
  # ==========================================

  def self.test_auth_bearer_valid_admin
    token = Base64.strict_encode64("admin:secret123")
    res = self.http_auth_get("/auth_bearer", { "Authorization" => "Bearer #{token}" })
    res.code == "200" && res["X-Auth-Status"] == "authorized"
  end

  def self.test_auth_bearer_valid_alice
    token = Base64.strict_encode64("alice:wonderland")
    res = self.http_auth_get("/auth_bearer", { "Authorization" => "Bearer #{token}" })
    res.code == "200" && res["X-Auth-Status"] == "authorized"
  end

  def self.test_auth_bearer_valid_bob
    token = Base64.strict_encode64("bob:builder")
    res = self.http_auth_get("/auth_bearer", { "Authorization" => "Bearer #{token}" })
    res.code == "200" && res["X-Auth-Status"] == "authorized"
  end

  # ==========================================
  # Authorize Context Rule - Basic Token Tests
  # ==========================================

  def self.test_auth_basic_valid
    token = Base64.strict_encode64("admin:secret123")
    res = self.http_auth_get("/auth_bearer", { "Authorization" => "Basic #{token}" })
    res.code == "200" && res["X-Auth-Status"] == "authorized"
  end

  # ==========================================
  # Authorize Context Rule - Negative & Edge Cases
  # ==========================================

  def self.test_auth_missing_header
    res = self.http_auth_get("/auth_bearer")
    res.code == "401"
  end

  def self.test_auth_empty_header
    res = self.http_auth_get("/auth_bearer", { "Authorization" => "" })
    res.code == "401"
  end

  def self.test_auth_invalid_password
    token = Base64.strict_encode64("admin:wrongpassword")
    res = self.http_auth_get("/auth_bearer", { "Authorization" => "Bearer #{token}" })
    res.code == "401"
  end

  def self.test_auth_unknown_user
    token = Base64.strict_encode64("nonexistent:secret123")
    res = self.http_auth_get("/auth_bearer", { "Authorization" => "Bearer #{token}" })
    res.code == "401"
  end

  def self.test_auth_malformed_base64
    res = self.http_auth_get("/auth_bearer", { "Authorization" => "Bearer !!!invalid_base64!!!" })
    res.code == "401"
  end

  def self.test_auth_malformed_header_no_token
    res = self.http_auth_get("/auth_bearer", { "Authorization" => "Bearer" })
    res.code == "401"
  end

  def self.test_auth_unsupported_scheme
    token = Base64.strict_encode64("admin:secret123")
    res = self.http_auth_get("/auth_bearer", { "Authorization" => "Digest #{token}" })
    res.code == "401"
  end

  # ==========================================
  # Authorize-Via Context Rule - Default Header
  # ==========================================

  def self.test_auth_via_default_header_success
    res = self.http_auth_get("/auth_via", { "Authorization" => "secret-token-123" })
    res.code == "200" && res["X-Auth-Via-Status"] == "authorized"
  end

  def self.test_auth_via_default_header_failure
    res = self.http_auth_get("/auth_via", { "Authorization" => "wrong-token" })
    res.code == "401"
  end

  def self.test_auth_via_missing_header
    res = self.http_auth_get("/auth_via")
    res.code == "401"
  end

  # ==========================================
  # Authorize-Via Context Rule - Custom Header
  # ==========================================

  def self.test_auth_via_custom_header_success
    res = self.http_auth_get("/auth_custom_header", { "X-Api-Key" => "secret-token-123" })
    res.code == "200" && res["X-Auth-Via-Custom-Status"] == "authorized"
  end

  def self.test_auth_via_custom_header_failure
    res = self.http_auth_get("/auth_custom_header", { "X-Api-Key" => "invalid-key" })
    res.code == "401"
  end

  def self.test_auth_via_custom_header_missing
    res = self.http_auth_get("/auth_custom_header")
    res.code == "401"
  end
end
