# What's In Your Water? — Hawaiʻi Ocean Sampling Dashboard

A public-facing Shiny app tracking coastal water quality across Oʻahu and Maui Nui after the March 2026 Kona Low storm.

**Live app:** *(add your shinyapps.io URL here)*

---

## Routine maintenance

### Updating the samples analyzed count

The progress bar on the "What are we measuring?" page reads `samples_analyzed` from a **Config** tab in the Google Sheet. To update it:

1. Open the Google Sheet
2. Go to the **Config** tab (create it if it doesn't exist — see setup below)
3. Change the value in the `value` column next to `samples_analyzed`
4. The app picks it up automatically within 5 minutes (no redeploy needed)

**Config tab setup** (one-time):
Create a sheet tab named `Config` with two columns and one data row:

| key              | value |
|------------------|-------|
| samples_analyzed | 0     |

---

### When the app shows stale data

The app re-reads the Google Sheet every 5 minutes automatically. If you need an immediate refresh, restart the app from your shinyapps.io dashboard:

1. Log in to [shinyapps.io](https://www.shinyapps.io)
2. Go to **Applications** → select the app
3. Click **Restart**

---

### Re-authenticating Google Sheets (token expiry)

The Google OAuth token stored in `.secrets/` typically lasts several months. When it expires, the app will crash with a 403 error. To fix:

1. In RStudio, run:
   ```r
   options(gargle_oauth_cache = ".secrets")
   gs4_auth(email = "silbiger@hawaii.edu")
   ```
2. Approve access in the browser window that opens
3. Redeploy the app (see below) — the refreshed token in `.secrets/` will be bundled with it

> ⚠️ Never commit `.secrets/` to GitHub. It is in `.gitignore` for this reason.

---

### Redeploying the app

```r
library(rsconnect)
deployApp()
```

Or use the **Publish** button in RStudio (top right of the editor).

---

## Local development

```r
# Install dependencies if needed
install.packages(c("shiny", "bslib", "tidyverse", "googlesheets4",
                   "lubridate", "leaflet", "fontawesome"))

# Authenticate (first time only)
options(gargle_oauth_cache = ".secrets")
gs4_auth(email = "silbiger@hawaii.edu")

# Run the app
shiny::runApp()
```

---

## Contact

| Name | Email | Lab |
|------|-------|-----|
| Dr. Nyssa Silbiger | silbiger@hawaii.edu | [silbigerlab.com](https://silbigerlab.com) |
| Dr. Andrea Kealoha | andreake@hawaii.edu | [andreakealoha](https://andreake6.wixsite.com/andreakealoha) |
| Dr. Sara Kahanamoku | sara.kahanamoku@hawaii.edu | [skahanamoku.com](https://www.skahanamoku.com/) |
