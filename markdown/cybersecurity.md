## Security Features

- Input validation with file type/size limits
- Malicious content detection
- Rate limiting: 100 requests/hour per session
- Security headers: CSP, XSS prevention, clickjacking protection
- HTTPS encryption: TLS 1.2+ for all data transmission
- Audit logging in `security.log`

## API Key Management

- `.env` file support (auto-detected)
- Masked input field in app
- Environment variable: `Sys.setenv(OPENAI_API_KEY = "...")`
- Format validated, never logged, session-isolated

## Infrastructure

- Cloudflare DNS with DDoS protection
- Docker + Nginx deployment
- Session-based with no persistent data storage
- No cookies or persistent identifiers

## Privacy & Data Protection

- Input validation and sanitization
- Secure session management
- Local processing (R Package): FERPA and HIPAA compliant
- Offline capable with optional local AI (Ollama)
