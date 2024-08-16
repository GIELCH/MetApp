# MetApp

## Brief description 🔍
MetApp is a Shiny application dedicated to [MetIDfyR](https://github.com/agnesblch/MetIDfyR). It allows users to generate input and configuration files, launch a run and visualize the output. The Visualization tab displays a table with all putative metabolites identified in the mzML data. You can save a PDF report with selected hits.

## Good to know 💡
- Default path to the **MetIDfyR directory** is set as "/usr/MetIDfyR" at the beginning of the server.R file.
- Default path to the **output directories** in the Visualization tab is set as "/datas/output_MetIDfyR/" at the end of the server.R file.
- Input and configuration files generated from the app will be saved in "/usr/MetIDfyR/input" by default.

📝 Do not hesitate to modify those paths to match your configuration!
