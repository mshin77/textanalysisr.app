## AI Integration

TextAnalysisR provides AI/NLP capabilities via local and web-based providers.

### Providers

<table style="width: 100%; border-collapse: collapse; margin-bottom: 15px;">
<thead>
<tr style="border-bottom: 1px solid #dee2e6;">
<th style="text-align: left; padding: 8px;">Provider</th>
<th style="text-align: left; padding: 8px;">Type</th>
<th style="text-align: left; padding: 8px;">Best For</th>
</tr>
</thead>
<tbody>
<tr style="border-bottom: 1px solid #dee2e6;">
<td style="padding: 8px;"><a href="https://ollama.com" target="_blank">Ollama</a></td>
<td style="padding: 8px;">Local</td>
<td style="padding: 8px;">Privacy, no cost</td>
</tr>
<tr style="border-bottom: 1px solid #dee2e6;">
<td style="padding: 8px;"><a href="https://platform.openai.com/" target="_blank">OpenAI</a></td>
<td style="padding: 8px;">Web-based</td>
<td style="padding: 8px;">Quality, speed</td>
</tr>
<tr style="border-bottom: 1px solid #dee2e6;">
<td style="padding: 8px;"><a href="https://ai.google.dev/" target="_blank">Gemini</a></td>
<td style="padding: 8px;">Web-based</td>
<td style="padding: 8px;">Quality, speed</td>
</tr>
</tbody>
</table>

### Features

- **Topic Labels**: AI-suggested labels from topic model terms
- **Content Generation**: Survey items, research questions, themes
- **Cluster Labels**: AI-suggested names for document clusters
- **RAG Search**: Question-answering over your corpus
- **Vision AI**: PDF image/chart extraction via Ollama, OpenAI, or Gemini
- **Sentiment Analysis**: LLM-based or local transformer models
- **Linguistic Analysis**: POS, NER, dependency parsing via spaCy

### Setup

```r
# Local AI (recommended for privacy)
# Install Ollama: https://ollama.com
# Then: ollama pull llama3.2

# Web-based AI
Sys.setenv(OPENAI_API_KEY = "your-key")
# or
Sys.setenv(GEMINI_API_KEY = "your-key")
```

### Responsible AI

All AI features follow human-in-the-loop design: AI suggests, you review and approve.

See [AI Integration vignette](https://mshin77.github.io/TextAnalysisR/articles/ai_integration.html) for detailed documentation.
