## Input Validation

- File uploads: Extension whitelist, 50MB limit, malicious content scanning
- Text and LLM inputs: XSS and prompt injection filtering
- Column names: Regex validation to prevent formula injection

## API Key Security

- Stored via `.env` or environment variables (never logged or persisted)
- Masked input, format validation, transmitted via secure headers only

## Network Security

- Content Security Policy, X-Frame-Options, SRI for CDN resources
- HTTPS with TLS 1.2+ via Nginx/Cloudflare

## Data Protection

- Session-scoped with no persistent storage, cookies, or identifiers
- Rate limiting: 100 requests/hour per session
- Security event logging with sanitized error messages
- Local processing option (FERPA/HIPAA compatible)
- Offline AI via Ollama for fully local operation

## Infrastructure

- Cloudflare DNS with DDoS protection
- Docker + Nginx deployment
