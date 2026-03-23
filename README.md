# Research Lab Dashboard

A static GitHub Pages dashboard for tracking ongoing and completed research experiments, activity trends, and publications. All content is driven by a single `data.json` file — no build tools or backend required.

---

## Setup

1. **Create a new GitHub repository** (e.g. `your-lab.github.io` or `research-dashboard`)
2. Upload `index.html` and `data.json` to the root of the repo
3. Go to **Settings → Pages → Source** and select `main` branch / root
4. Your dashboard will be live at `https://your-username.github.io/repo-name`

---

## Updating the Dashboard

Edit `data.json` and commit — the site updates automatically.

### Site metadata
```json
"site": {
  "title": "Your Lab Name",
  "subtitle": "Brief project description",
  "institution": "University · Department",
  "updated": "2026-03-23"
}
```
Update `"updated"` to today's date whenever you push changes.

---

### Stats (top summary numbers)
```json
"stats": [
  { "label": "Active Experiments", "value": "4" },
  { "label": "Datasets Collected", "value": "12" }
]
```
Add, remove, or rename any stat. `"value"` is always a string (can include symbols like `"~12"` or `">100"`).

---

### Experiments

Each experiment entry:
```json
{
  "id": "EXP-005",
  "title": "Your experiment title",
  "status": "ongoing",       // "ongoing" | "completed" | "planned"
  "start": "2026-03",
  "end": null,               // null if still running, or "2026-09"
  "progress": 45,            // 0–100 (integer)
  "description": "One or two sentence summary.",
  "tags": ["method", "topic"]
}
```
Add new experiments to the `"experiments"` array. Completed ones will turn blue automatically.

---

### Activity Chart

Edit the `"chart"` block to update the bar chart:
```json
"chart": {
  "label": "Samples Processed per Month",
  "months": ["Oct", "Nov", "Dec", "Jan", "Feb", "Mar"],
  "values": [18, 24, 14, 31, 27, 35]
}
```
`months` and `values` must have the same length. To add a new month, append to both arrays and remove the oldest entry if you want to keep it to 6 bars.

---

### Publications

```json
{
  "year": 2026,
  "title": "Full paper title",
  "authors": "Last F, Last F, et al.",
  "journal": "Journal Name",
  "doi": "10.xxxx/xxxxx",   // or null if not yet available
  "status": "published"     // "published" | "preprint"
}
```
Add new publications to the top of the `"publications"` array to keep them in reverse-chronological order.

---

## Workflow

```
# Clone your repo
git clone https://github.com/your-username/research-dashboard.git
cd research-dashboard

# Edit data.json, then push
git add data.json
git commit -m "Add EXP-005, update progress"
git push
```

GitHub Pages will rebuild within ~30 seconds of your push.

---

## File Structure

```
/
├── index.html   ← Dashboard UI (edit only for layout/style changes)
├── data.json    ← All your content lives here
└── README.md    ← This file
```
