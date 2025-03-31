# Global Earthquake Events (1995-2023)

## This interactive app and dataset shows earthquake distributions from 1995-2023. It captures critical variables used in our analysis such as Magnitude, CDI (Community Decimal Intensity), MMI (Modified Mercalli Intensity), SIG (Significance), and Depth. We utilize a variety of carefully chosen colors and themes to represent within our interactive visuals and app, with the use of incorporating the multiple critical eartquake measurements and variables together, in order to create a dynamic flow of information and various live-time interactions with the user. 

<img src="Images/Sankey Diagram.png" width=1000>

This analysis aims to gain a deeper understanding of earthquake patterns and their potential impact and catastrophic effects across different regions and communities. By examining variables such as magnitude, depth, and intensity, we explore trends, identify high-risk areas, and assess the potential for future great earthquakes. By studying a few major events alongside broader global patterns, we aim to derive critical lessons to improve disaster preparedness, response systems, and resilience strategies on a worldwide scale. 

<img width="1430" alt="Screenshot 2024-11-14 at 5 11 43 PM" src="https://github.com/user-attachments/assets/3e382bb5-73e9-4956-952d-bc3e203df7f9">

<img src="Images/Radar Chart.png" width=1000>

[View the app on shinyapps.io](https://charchar.shinyapps.io/Earthquakes_CA/)

To run in your R Console: 

library(shiny)

runGitHub(repo="Earthquakes", username="CharlotteGAnderson", ref="main")
