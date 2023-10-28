<h1>Cyclistic Bike Share Analysis</h1>



<h2>Business Task</h2>
The goal of this project is to analyze how annual members and casual riders of a bike share company in Chicago use cyclistic bikes differently in order to design marketing strategies aimed at converting casual riders to annual members. Data over a period of 12 months would be explored and sourced from Cyclistic's historical bike trip records. A detailed report with desired deliverables, which includes data cleaning and manipulation documentation, analysis summary, supporting visualizations, key findings, and top three recommendations, will be produced.

<br />


<h2>Description of Data Source</h2>
The data source I used is historical trip data made available by Motivate International Inc. I focused on data from a period of 12 months dating from June 2022 to May 2023. Each monthly dataset contains ride ids, type of bike used, start and end datetimes, start and end station names, start and end station ids, latitude and longitude coordinates for start and end stations, and the type of user (either casual or member). To start, I sorted the data based on membership and removed the latitude and longitude coordinates as those were not needed. The data is public. Data privacy issues prohibit the use of riders personal information such as credit card numbers. 

<h2>Documentation of Data Cleaning/Transformation </h2>

## Version 1.0.0 (09-29-2023)

New

- Added a new column of "day_of_week" to track the day of the week each ride started
- Added a new column of "ride_length_hms" that contains the time length from "started_at" and "ended_at"

Changes

- Combined all dataframes into one large dataframe
- Ordered each data set by type of member_casual (casual, member)

Removed

- Removed unncessary columns (start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng)

<h2>Program walk-through:</h2>

<p align="center">
Launch the utility: <br/>
<img src="https://i.imgur.com/62TgaWL.png" height="80%" width="80%" alt="Disk Sanitization Steps"/>
<br />
<br />
Select the disk:  <br/>
<img src="https://i.imgur.com/tcTyMUE.png" height="80%" width="80%" alt="Disk Sanitization Steps"/>
<br />
<br />
Enter the number of passes: <br/>
<img src="https://i.imgur.com/nCIbXbg.png" height="80%" width="80%" alt="Disk Sanitization Steps"/>
<br />
<br />
Confirm your selection:  <br/>
<img src="https://i.imgur.com/cdFHBiU.png" height="80%" width="80%" alt="Disk Sanitization Steps"/>
<br />
<br />
Wait for process to complete (may take some time):  <br/>
<img src="https://i.imgur.com/JL945Ga.png" height="80%" width="80%" alt="Disk Sanitization Steps"/>
<br />
<br />
Sanitization complete:  <br/>
<img src="https://i.imgur.com/K71yaM2.png" height="80%" width="80%" alt="Disk Sanitization Steps"/>
<br />
<br />
Observe the wiped disk:  <br/>
<img src="https://i.imgur.com/AeZkvFQ.png" height="80%" width="80%" alt="Disk Sanitization Steps"/>
</p>

<!--
 ```diff
- text in red
+ text in green
! text in orange
# text in gray
@@ text in purple (and bold)@@
```
--!>
