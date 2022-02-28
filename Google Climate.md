Install the Packages.
```R
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("data.table")
install.packages("tidyverse")
install.packages("reshape2")
```
Load the Packages.
 
```R
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
clim_data = read.csv("D:/Downloads/GoogleTemps/GlobalLandTemperaturesByState.csv", header = TRUE)
```
 
Filter the data to the US and separate dt into Year, Month, and Day.

```R
clim_data %>%
    filter(Country == "United States") %>%
    separate(col = dt, into = c("Year", "Month", "Day"), convert = TRUE) -> clim_data  
clim_data = na.omit(clim_data)
```


```R
head(clim_data)
```


<table>
<thead><tr><th></th><th scope=col>Year</th><th scope=col>Month</th><th scope=col>Day</th><th scope=col>AverageTemperature</th><th scope=col>AverageTemperatureUncertainty</th><th scope=col>State</th><th scope=col>Country</th></tr></thead>
<tbody>
	<tr><th scope=row>1</th><td>1743         </td><td>11           </td><td>1            </td><td>10.722       </td><td>2.898        </td><td>Alabama      </td><td>United States</td></tr>
	<tr><th scope=row>6</th><td>1744         </td><td> 4           </td><td>1            </td><td>19.075       </td><td>2.902        </td><td>Alabama      </td><td>United States</td></tr>
	<tr><th scope=row>7</th><td>1744         </td><td> 5           </td><td>1            </td><td>21.197       </td><td>2.844        </td><td>Alabama      </td><td>United States</td></tr>
	<tr><th scope=row>8</th><td>1744         </td><td> 6           </td><td>1            </td><td>25.290       </td><td>2.879        </td><td>Alabama      </td><td>United States</td></tr>
	<tr><th scope=row>9</th><td>1744         </td><td> 7           </td><td>1            </td><td>26.420       </td><td>2.841        </td><td>Alabama      </td><td>United States</td></tr>
	<tr><th scope=row>11</th><td>1744         </td><td> 9           </td><td>1            </td><td>21.735       </td><td>2.866        </td><td>Alabama      </td><td>United States</td></tr>
</tbody>
</table>




```R
tail(clim_data)
```


<table>
<thead><tr><th></th><th scope=col>Year</th><th scope=col>Month</th><th scope=col>Day</th><th scope=col>AverageTemperature</th><th scope=col>AverageTemperatureUncertainty</th><th scope=col>State</th><th scope=col>Country</th></tr></thead>
<tbody>
	<tr><th scope=row>149740</th><td>2013         </td><td>4            </td><td>1            </td><td> 1.673       </td><td>0.282        </td><td>Wyoming      </td><td>United States</td></tr>
	<tr><th scope=row>149741</th><td>2013         </td><td>5            </td><td>1            </td><td>10.607       </td><td>0.208        </td><td>Wyoming      </td><td>United States</td></tr>
	<tr><th scope=row>149742</th><td>2013         </td><td>6            </td><td>1            </td><td>16.267       </td><td>0.276        </td><td>Wyoming      </td><td>United States</td></tr>
	<tr><th scope=row>149743</th><td>2013         </td><td>7            </td><td>1            </td><td>20.222       </td><td>0.133        </td><td>Wyoming      </td><td>United States</td></tr>
	<tr><th scope=row>149744</th><td>2013         </td><td>8            </td><td>1            </td><td>19.621       </td><td>0.217        </td><td>Wyoming      </td><td>United States</td></tr>
	<tr><th scope=row>149745</th><td>2013         </td><td>9            </td><td>1            </td><td>15.811       </td><td>1.101        </td><td>Wyoming      </td><td>United States</td></tr>
</tbody>
</table>


Filter by a more "recent" year and group by the year for easy visualization.

```R
clim_data %>%
filter(Year>1900) %>%
group_by(Year) %>%
summarise(Temp = mean(AverageTemperature)) -> clim_data2
head(clim_data2)
```


<table>
<thead><tr><th scope=col>Year</th><th scope=col>Temp</th></tr></thead>
<tbody>
	<tr><td>1901     </td><td>10.531281</td></tr>
	<tr><td>1902     </td><td>10.568482</td></tr>
	<tr><td>1903     </td><td>10.174247</td></tr>
	<tr><td>1904     </td><td> 9.961833</td></tr>
	<tr><td>1905     </td><td>10.272663</td></tr>
	<tr><td>1906     </td><td>10.768693</td></tr>
</tbody>
</table>


Plot the the data to visualize temperature change in the US. 

```R
qplot(Year, Temp, data=clim_data2, main="Average Temperature 1900-2013", geom=c("point","smooth")) + aes(color = Temp) + scale_color_gradient(low="grey", high="red")
```

    `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    


    
![png](output_6_1.png)
    
Group the data by state and year. This step will make it easy to pull specific states out of the data. I am going to look at the PNW.

```R
clim_data %>%
select(Year, AverageTemperature, State) %>%
group_by(Year,State) %>%
summarize(value = mean(AverageTemperature), .groups = 'drop') -> clim_data1

colnames(clim_data1)[2] <- "region" 
clim_data1$region<-tolower(clim_data1$region)

clim_data1 %>%
filter(Year==1900) -> clim_data1900
clim_data1900<-clim_data1900[,1:3]
```


```R
clim_data1900
```


<table>
<thead><tr><th scope=col>Year</th><th scope=col>region</th><th scope=col>value</th></tr></thead>
<tbody>
	<tr><td>1900                </td><td>alabama             </td><td>17.059167           </td></tr>
	<tr><td>1900                </td><td>alaska              </td><td>-5.146500           </td></tr>
	<tr><td>1900                </td><td>arizona             </td><td>15.742917           </td></tr>
	<tr><td>1900                </td><td>arkansas            </td><td>15.893417           </td></tr>
	<tr><td>1900                </td><td>california          </td><td>14.515750           </td></tr>
	<tr><td>1900                </td><td>colorado            </td><td> 7.470917           </td></tr>
	<tr><td>1900                </td><td>connecticut         </td><td> 9.763167           </td></tr>
	<tr><td>1900                </td><td>delaware            </td><td>12.690167           </td></tr>
	<tr><td>1900                </td><td>district of columbia</td><td>12.712083           </td></tr>
	<tr><td>1900                </td><td>florida             </td><td>21.344750           </td></tr>
	<tr><td>1900                </td><td>georgia (state)     </td><td>17.492667           </td></tr>
	<tr><td>1900                </td><td>hawaii              </td><td>22.429250           </td></tr>
	<tr><td>1900                </td><td>idaho               </td><td> 6.275917           </td></tr>
	<tr><td>1900                </td><td>illinois            </td><td>11.423833           </td></tr>
	<tr><td>1900                </td><td>indiana             </td><td>11.446667           </td></tr>
	<tr><td>1900                </td><td>iowa                </td><td> 9.322500           </td></tr>
	<tr><td>1900                </td><td>kansas              </td><td>12.764667           </td></tr>
	<tr><td>1900                </td><td>kentucky            </td><td>13.414750           </td></tr>
	<tr><td>1900                </td><td>louisiana           </td><td>19.020583           </td></tr>
	<tr><td>1900                </td><td>maine               </td><td> 4.675333           </td></tr>
	<tr><td>1900                </td><td>maryland            </td><td>12.896167           </td></tr>
	<tr><td>1900                </td><td>massachusetts       </td><td> 8.245833           </td></tr>
	<tr><td>1900                </td><td>michigan            </td><td> 7.180833           </td></tr>
	<tr><td>1900                </td><td>minnesota           </td><td> 5.405583           </td></tr>
	<tr><td>1900                </td><td>mississippi         </td><td>17.587417           </td></tr>
	<tr><td>1900                </td><td>missouri            </td><td>12.810083           </td></tr>
	<tr><td>1900                </td><td>montana             </td><td> 6.094750           </td></tr>
	<tr><td>1900                </td><td>nebraska            </td><td>10.057500           </td></tr>
	<tr><td>1900                </td><td>nevada              </td><td>10.332917           </td></tr>
	<tr><td>1900                </td><td>new hampshire       </td><td> 6.001583           </td></tr>
	<tr><td>1900                </td><td>new jersey          </td><td>11.463833           </td></tr>
	<tr><td>1900                </td><td>new mexico          </td><td>12.172417           </td></tr>
	<tr><td>1900                </td><td>new york            </td><td> 7.859583           </td></tr>
	<tr><td>1900                </td><td>north carolina      </td><td>15.137750           </td></tr>
	<tr><td>1900                </td><td>north dakota        </td><td> 5.195500           </td></tr>
	<tr><td>1900                </td><td>ohio                </td><td>10.906667           </td></tr>
	<tr><td>1900                </td><td>oklahoma            </td><td>15.558417           </td></tr>
	<tr><td>1900                </td><td>oregon              </td><td> 8.671583           </td></tr>
	<tr><td>1900                </td><td>pennsylvania        </td><td> 9.888417           </td></tr>
	<tr><td>1900                </td><td>rhode island        </td><td> 9.702167           </td></tr>
	<tr><td>1900                </td><td>south carolina      </td><td>17.147167           </td></tr>
	<tr><td>1900                </td><td>south dakota        </td><td> 8.186000           </td></tr>
	<tr><td>1900                </td><td>tennessee           </td><td>14.341667           </td></tr>
	<tr><td>1900                </td><td>texas               </td><td>17.984167           </td></tr>
	<tr><td>1900                </td><td>utah                </td><td> 9.206917           </td></tr>
	<tr><td>1900                </td><td>vermont             </td><td> 5.781917           </td></tr>
	<tr><td>1900                </td><td>virginia            </td><td>13.428917           </td></tr>
	<tr><td>1900                </td><td>washington          </td><td> 8.185667           </td></tr>
	<tr><td>1900                </td><td>west virginia       </td><td>11.528750           </td></tr>
	<tr><td>1900                </td><td>wisconsin           </td><td> 6.624167           </td></tr>
	<tr><td>1900                </td><td>wyoming             </td><td> 6.130667           </td></tr>
</tbody>
</table>




```R
clim_data1 %>%
filter(Year==2000) -> clim_data2000
clim_data2000<-clim_data2000[,1:3]
```


```R
clim_data2000
```


<table>
<thead><tr><th scope=col>Year</th><th scope=col>region</th><th scope=col>value</th></tr></thead>
<tbody>
	<tr><td>2000                </td><td>alabama             </td><td>17.911333           </td></tr>
	<tr><td>2000                </td><td>alaska              </td><td>-3.301750           </td></tr>
	<tr><td>2000                </td><td>arizona             </td><td>16.510250           </td></tr>
	<tr><td>2000                </td><td>arkansas            </td><td>16.362583           </td></tr>
	<tr><td>2000                </td><td>california          </td><td>15.019333           </td></tr>
	<tr><td>2000                </td><td>colorado            </td><td> 8.204833           </td></tr>
	<tr><td>2000                </td><td>connecticut         </td><td> 9.581000           </td></tr>
	<tr><td>2000                </td><td>delaware            </td><td>12.347583           </td></tr>
	<tr><td>2000                </td><td>district of columbia</td><td>12.277750           </td></tr>
	<tr><td>2000                </td><td>florida             </td><td>21.952000           </td></tr>
	<tr><td>2000                </td><td>georgia (state)     </td><td>17.920667           </td></tr>
	<tr><td>2000                </td><td>hawaii              </td><td>22.941500           </td></tr>
	<tr><td>2000                </td><td>idaho               </td><td> 6.239000           </td></tr>
	<tr><td>2000                </td><td>illinois            </td><td>11.582917           </td></tr>
	<tr><td>2000                </td><td>indiana             </td><td>11.415917           </td></tr>
	<tr><td>2000                </td><td>iowa                </td><td> 9.809667           </td></tr>
	<tr><td>2000                </td><td>kansas              </td><td>13.343583           </td></tr>
	<tr><td>2000                </td><td>kentucky            </td><td>13.479500           </td></tr>
	<tr><td>2000                </td><td>louisiana           </td><td>19.913250           </td></tr>
	<tr><td>2000                </td><td>maine               </td><td> 5.210583           </td></tr>
	<tr><td>2000                </td><td>maryland            </td><td>12.513333           </td></tr>
	<tr><td>2000                </td><td>massachusetts       </td><td> 8.286750           </td></tr>
	<tr><td>2000                </td><td>michigan            </td><td> 7.579833           </td></tr>
	<tr><td>2000                </td><td>minnesota           </td><td> 5.710250           </td></tr>
	<tr><td>2000                </td><td>mississippi         </td><td>18.429333           </td></tr>
	<tr><td>2000                </td><td>missouri            </td><td>13.196750           </td></tr>
	<tr><td>2000                </td><td>montana             </td><td> 5.835833           </td></tr>
	<tr><td>2000                </td><td>nebraska            </td><td>10.194667           </td></tr>
	<tr><td>2000                </td><td>nevada              </td><td>10.847500           </td></tr>
	<tr><td>2000                </td><td>new hampshire       </td><td> 6.291000           </td></tr>
	<tr><td>2000                </td><td>new jersey          </td><td>11.076500           </td></tr>
	<tr><td>2000                </td><td>new mexico          </td><td>13.075750           </td></tr>
	<tr><td>2000                </td><td>new york            </td><td> 7.737917           </td></tr>
	<tr><td>2000                </td><td>north carolina      </td><td>15.099917           </td></tr>
	<tr><td>2000                </td><td>north dakota        </td><td> 5.364250           </td></tr>
	<tr><td>2000                </td><td>ohio                </td><td>10.875333           </td></tr>
	<tr><td>2000                </td><td>oklahoma            </td><td>16.142583           </td></tr>
	<tr><td>2000                </td><td>oregon              </td><td> 8.678000           </td></tr>
	<tr><td>2000                </td><td>pennsylvania        </td><td> 9.674833           </td></tr>
	<tr><td>2000                </td><td>rhode island        </td><td> 9.552917           </td></tr>
	<tr><td>2000                </td><td>south carolina      </td><td>17.322833           </td></tr>
	<tr><td>2000                </td><td>south dakota        </td><td> 8.096167           </td></tr>
	<tr><td>2000                </td><td>tennessee           </td><td>14.701333           </td></tr>
	<tr><td>2000                </td><td>texas               </td><td>19.238000           </td></tr>
	<tr><td>2000                </td><td>utah                </td><td> 9.761167           </td></tr>
	<tr><td>2000                </td><td>vermont             </td><td> 6.032417           </td></tr>
	<tr><td>2000                </td><td>virginia            </td><td>13.203583           </td></tr>
	<tr><td>2000                </td><td>washington          </td><td> 7.859917           </td></tr>
	<tr><td>2000                </td><td>west virginia       </td><td>11.466083           </td></tr>
	<tr><td>2000                </td><td>wisconsin           </td><td> 7.023667           </td></tr>
	<tr><td>2000                </td><td>wyoming             </td><td> 6.087917           </td></tr>
</tbody>
</table>


Merge the two datasets for visualization.

```R
df <- merge(clim_data1900, clim_data2000, by='region')
df
```


<table>
<thead><tr><th scope=col>region</th><th scope=col>Year.x</th><th scope=col>value.x</th><th scope=col>Year.y</th><th scope=col>value.y</th></tr></thead>
<tbody>
	<tr><td>alabama             </td><td>1900                </td><td>17.059167           </td><td>2000                </td><td>17.911333           </td></tr>
	<tr><td>alaska              </td><td>1900                </td><td>-5.146500           </td><td>2000                </td><td>-3.301750           </td></tr>
	<tr><td>arizona             </td><td>1900                </td><td>15.742917           </td><td>2000                </td><td>16.510250           </td></tr>
	<tr><td>arkansas            </td><td>1900                </td><td>15.893417           </td><td>2000                </td><td>16.362583           </td></tr>
	<tr><td>california          </td><td>1900                </td><td>14.515750           </td><td>2000                </td><td>15.019333           </td></tr>
	<tr><td>colorado            </td><td>1900                </td><td> 7.470917           </td><td>2000                </td><td> 8.204833           </td></tr>
	<tr><td>connecticut         </td><td>1900                </td><td> 9.763167           </td><td>2000                </td><td> 9.581000           </td></tr>
	<tr><td>delaware            </td><td>1900                </td><td>12.690167           </td><td>2000                </td><td>12.347583           </td></tr>
	<tr><td>district of columbia</td><td>1900                </td><td>12.712083           </td><td>2000                </td><td>12.277750           </td></tr>
	<tr><td>florida             </td><td>1900                </td><td>21.344750           </td><td>2000                </td><td>21.952000           </td></tr>
	<tr><td>georgia (state)     </td><td>1900                </td><td>17.492667           </td><td>2000                </td><td>17.920667           </td></tr>
	<tr><td>hawaii              </td><td>1900                </td><td>22.429250           </td><td>2000                </td><td>22.941500           </td></tr>
	<tr><td>idaho               </td><td>1900                </td><td> 6.275917           </td><td>2000                </td><td> 6.239000           </td></tr>
	<tr><td>illinois            </td><td>1900                </td><td>11.423833           </td><td>2000                </td><td>11.582917           </td></tr>
	<tr><td>indiana             </td><td>1900                </td><td>11.446667           </td><td>2000                </td><td>11.415917           </td></tr>
	<tr><td>iowa                </td><td>1900                </td><td> 9.322500           </td><td>2000                </td><td> 9.809667           </td></tr>
	<tr><td>kansas              </td><td>1900                </td><td>12.764667           </td><td>2000                </td><td>13.343583           </td></tr>
	<tr><td>kentucky            </td><td>1900                </td><td>13.414750           </td><td>2000                </td><td>13.479500           </td></tr>
	<tr><td>louisiana           </td><td>1900                </td><td>19.020583           </td><td>2000                </td><td>19.913250           </td></tr>
	<tr><td>maine               </td><td>1900                </td><td> 4.675333           </td><td>2000                </td><td> 5.210583           </td></tr>
	<tr><td>maryland            </td><td>1900                </td><td>12.896167           </td><td>2000                </td><td>12.513333           </td></tr>
	<tr><td>massachusetts       </td><td>1900                </td><td> 8.245833           </td><td>2000                </td><td> 8.286750           </td></tr>
	<tr><td>michigan            </td><td>1900                </td><td> 7.180833           </td><td>2000                </td><td> 7.579833           </td></tr>
	<tr><td>minnesota           </td><td>1900                </td><td> 5.405583           </td><td>2000                </td><td> 5.710250           </td></tr>
	<tr><td>mississippi         </td><td>1900                </td><td>17.587417           </td><td>2000                </td><td>18.429333           </td></tr>
	<tr><td>missouri            </td><td>1900                </td><td>12.810083           </td><td>2000                </td><td>13.196750           </td></tr>
	<tr><td>montana             </td><td>1900                </td><td> 6.094750           </td><td>2000                </td><td> 5.835833           </td></tr>
	<tr><td>nebraska            </td><td>1900                </td><td>10.057500           </td><td>2000                </td><td>10.194667           </td></tr>
	<tr><td>nevada              </td><td>1900                </td><td>10.332917           </td><td>2000                </td><td>10.847500           </td></tr>
	<tr><td>new hampshire       </td><td>1900                </td><td> 6.001583           </td><td>2000                </td><td> 6.291000           </td></tr>
	<tr><td>new jersey          </td><td>1900                </td><td>11.463833           </td><td>2000                </td><td>11.076500           </td></tr>
	<tr><td>new mexico          </td><td>1900                </td><td>12.172417           </td><td>2000                </td><td>13.075750           </td></tr>
	<tr><td>new york            </td><td>1900                </td><td> 7.859583           </td><td>2000                </td><td> 7.737917           </td></tr>
	<tr><td>north carolina      </td><td>1900                </td><td>15.137750           </td><td>2000                </td><td>15.099917           </td></tr>
	<tr><td>north dakota        </td><td>1900                </td><td> 5.195500           </td><td>2000                </td><td> 5.364250           </td></tr>
	<tr><td>ohio                </td><td>1900                </td><td>10.906667           </td><td>2000                </td><td>10.875333           </td></tr>
	<tr><td>oklahoma            </td><td>1900                </td><td>15.558417           </td><td>2000                </td><td>16.142583           </td></tr>
	<tr><td>oregon              </td><td>1900                </td><td> 8.671583           </td><td>2000                </td><td> 8.678000           </td></tr>
	<tr><td>pennsylvania        </td><td>1900                </td><td> 9.888417           </td><td>2000                </td><td> 9.674833           </td></tr>
	<tr><td>rhode island        </td><td>1900                </td><td> 9.702167           </td><td>2000                </td><td> 9.552917           </td></tr>
	<tr><td>south carolina      </td><td>1900                </td><td>17.147167           </td><td>2000                </td><td>17.322833           </td></tr>
	<tr><td>south dakota        </td><td>1900                </td><td> 8.186000           </td><td>2000                </td><td> 8.096167           </td></tr>
	<tr><td>tennessee           </td><td>1900                </td><td>14.341667           </td><td>2000                </td><td>14.701333           </td></tr>
	<tr><td>texas               </td><td>1900                </td><td>17.984167           </td><td>2000                </td><td>19.238000           </td></tr>
	<tr><td>utah                </td><td>1900                </td><td> 9.206917           </td><td>2000                </td><td> 9.761167           </td></tr>
	<tr><td>vermont             </td><td>1900                </td><td> 5.781917           </td><td>2000                </td><td> 6.032417           </td></tr>
	<tr><td>virginia            </td><td>1900                </td><td>13.428917           </td><td>2000                </td><td>13.203583           </td></tr>
	<tr><td>washington          </td><td>1900                </td><td> 8.185667           </td><td>2000                </td><td> 7.859917           </td></tr>
	<tr><td>west virginia       </td><td>1900                </td><td>11.528750           </td><td>2000                </td><td>11.466083           </td></tr>
	<tr><td>wisconsin           </td><td>1900                </td><td> 6.624167           </td><td>2000                </td><td> 7.023667           </td></tr>
	<tr><td>wyoming             </td><td>1900                </td><td> 6.130667           </td><td>2000                </td><td> 6.087917           </td></tr>
</tbody>
</table>


Rename some of the columns. 

```R
df %>%
rename('1900' = Year.x,
      '2000' = Year.y) -> df
```


```R
df
```


<table>
<thead><tr><th scope=col>region</th><th scope=col>1900</th><th scope=col>value.x</th><th scope=col>2000</th><th scope=col>value.y</th></tr></thead>
<tbody>
	<tr><td>alabama             </td><td>1900                </td><td>17.059167           </td><td>2000                </td><td>17.911333           </td></tr>
	<tr><td>alaska              </td><td>1900                </td><td>-5.146500           </td><td>2000                </td><td>-3.301750           </td></tr>
	<tr><td>arizona             </td><td>1900                </td><td>15.742917           </td><td>2000                </td><td>16.510250           </td></tr>
	<tr><td>arkansas            </td><td>1900                </td><td>15.893417           </td><td>2000                </td><td>16.362583           </td></tr>
	<tr><td>california          </td><td>1900                </td><td>14.515750           </td><td>2000                </td><td>15.019333           </td></tr>
	<tr><td>colorado            </td><td>1900                </td><td> 7.470917           </td><td>2000                </td><td> 8.204833           </td></tr>
	<tr><td>connecticut         </td><td>1900                </td><td> 9.763167           </td><td>2000                </td><td> 9.581000           </td></tr>
	<tr><td>delaware            </td><td>1900                </td><td>12.690167           </td><td>2000                </td><td>12.347583           </td></tr>
	<tr><td>district of columbia</td><td>1900                </td><td>12.712083           </td><td>2000                </td><td>12.277750           </td></tr>
	<tr><td>florida             </td><td>1900                </td><td>21.344750           </td><td>2000                </td><td>21.952000           </td></tr>
	<tr><td>georgia (state)     </td><td>1900                </td><td>17.492667           </td><td>2000                </td><td>17.920667           </td></tr>
	<tr><td>hawaii              </td><td>1900                </td><td>22.429250           </td><td>2000                </td><td>22.941500           </td></tr>
	<tr><td>idaho               </td><td>1900                </td><td> 6.275917           </td><td>2000                </td><td> 6.239000           </td></tr>
	<tr><td>illinois            </td><td>1900                </td><td>11.423833           </td><td>2000                </td><td>11.582917           </td></tr>
	<tr><td>indiana             </td><td>1900                </td><td>11.446667           </td><td>2000                </td><td>11.415917           </td></tr>
	<tr><td>iowa                </td><td>1900                </td><td> 9.322500           </td><td>2000                </td><td> 9.809667           </td></tr>
	<tr><td>kansas              </td><td>1900                </td><td>12.764667           </td><td>2000                </td><td>13.343583           </td></tr>
	<tr><td>kentucky            </td><td>1900                </td><td>13.414750           </td><td>2000                </td><td>13.479500           </td></tr>
	<tr><td>louisiana           </td><td>1900                </td><td>19.020583           </td><td>2000                </td><td>19.913250           </td></tr>
	<tr><td>maine               </td><td>1900                </td><td> 4.675333           </td><td>2000                </td><td> 5.210583           </td></tr>
	<tr><td>maryland            </td><td>1900                </td><td>12.896167           </td><td>2000                </td><td>12.513333           </td></tr>
	<tr><td>massachusetts       </td><td>1900                </td><td> 8.245833           </td><td>2000                </td><td> 8.286750           </td></tr>
	<tr><td>michigan            </td><td>1900                </td><td> 7.180833           </td><td>2000                </td><td> 7.579833           </td></tr>
	<tr><td>minnesota           </td><td>1900                </td><td> 5.405583           </td><td>2000                </td><td> 5.710250           </td></tr>
	<tr><td>mississippi         </td><td>1900                </td><td>17.587417           </td><td>2000                </td><td>18.429333           </td></tr>
	<tr><td>missouri            </td><td>1900                </td><td>12.810083           </td><td>2000                </td><td>13.196750           </td></tr>
	<tr><td>montana             </td><td>1900                </td><td> 6.094750           </td><td>2000                </td><td> 5.835833           </td></tr>
	<tr><td>nebraska            </td><td>1900                </td><td>10.057500           </td><td>2000                </td><td>10.194667           </td></tr>
	<tr><td>nevada              </td><td>1900                </td><td>10.332917           </td><td>2000                </td><td>10.847500           </td></tr>
	<tr><td>new hampshire       </td><td>1900                </td><td> 6.001583           </td><td>2000                </td><td> 6.291000           </td></tr>
	<tr><td>new jersey          </td><td>1900                </td><td>11.463833           </td><td>2000                </td><td>11.076500           </td></tr>
	<tr><td>new mexico          </td><td>1900                </td><td>12.172417           </td><td>2000                </td><td>13.075750           </td></tr>
	<tr><td>new york            </td><td>1900                </td><td> 7.859583           </td><td>2000                </td><td> 7.737917           </td></tr>
	<tr><td>north carolina      </td><td>1900                </td><td>15.137750           </td><td>2000                </td><td>15.099917           </td></tr>
	<tr><td>north dakota        </td><td>1900                </td><td> 5.195500           </td><td>2000                </td><td> 5.364250           </td></tr>
	<tr><td>ohio                </td><td>1900                </td><td>10.906667           </td><td>2000                </td><td>10.875333           </td></tr>
	<tr><td>oklahoma            </td><td>1900                </td><td>15.558417           </td><td>2000                </td><td>16.142583           </td></tr>
	<tr><td>oregon              </td><td>1900                </td><td> 8.671583           </td><td>2000                </td><td> 8.678000           </td></tr>
	<tr><td>pennsylvania        </td><td>1900                </td><td> 9.888417           </td><td>2000                </td><td> 9.674833           </td></tr>
	<tr><td>rhode island        </td><td>1900                </td><td> 9.702167           </td><td>2000                </td><td> 9.552917           </td></tr>
	<tr><td>south carolina      </td><td>1900                </td><td>17.147167           </td><td>2000                </td><td>17.322833           </td></tr>
	<tr><td>south dakota        </td><td>1900                </td><td> 8.186000           </td><td>2000                </td><td> 8.096167           </td></tr>
	<tr><td>tennessee           </td><td>1900                </td><td>14.341667           </td><td>2000                </td><td>14.701333           </td></tr>
	<tr><td>texas               </td><td>1900                </td><td>17.984167           </td><td>2000                </td><td>19.238000           </td></tr>
	<tr><td>utah                </td><td>1900                </td><td> 9.206917           </td><td>2000                </td><td> 9.761167           </td></tr>
	<tr><td>vermont             </td><td>1900                </td><td> 5.781917           </td><td>2000                </td><td> 6.032417           </td></tr>
	<tr><td>virginia            </td><td>1900                </td><td>13.428917           </td><td>2000                </td><td>13.203583           </td></tr>
	<tr><td>washington          </td><td>1900                </td><td> 8.185667           </td><td>2000                </td><td> 7.859917           </td></tr>
	<tr><td>west virginia       </td><td>1900                </td><td>11.528750           </td><td>2000                </td><td>11.466083           </td></tr>
	<tr><td>wisconsin           </td><td>1900                </td><td> 6.624167           </td><td>2000                </td><td> 7.023667           </td></tr>
	<tr><td>wyoming             </td><td>1900                </td><td> 6.130667           </td><td>2000                </td><td> 6.087917           </td></tr>
</tbody>
</table>


Point plot for 1900 and 2000. Good way to see how the temperatures have varied over a century.

```R
ggplot(data=df) +
geom_point(mapping=aes(x=value.x,y=region,color='1900'))+
geom_point(mapping=aes(x=value.y,y=region,color='2000'))+
labs(title="1900 Temperatures vs 2000 Temperatures") +
xlab('Temperature in Celsius') +
ylab('State')

```


    
![png](output_14_0.png)
    

Filter by the PNW. In this case I am defining the PNW as Washington, Oregon, Montana, and Idaho.

```R
clim_data1 %>%
filter(region == 'washington' | region == 'oregon' | region == 'montana' | 
       region == 'idaho') -> pnw_data
pnw_data
```


<table>
<thead><tr><th scope=col>Year</th><th scope=col>region</th><th scope=col>value</th></tr></thead>
<tbody>
	<tr><td>1768     </td><td>montana  </td><td> 1.984750</td></tr>
	<tr><td>1769     </td><td>montana  </td><td> 6.991625</td></tr>
	<tr><td>1774     </td><td>montana  </td><td>-2.619000</td></tr>
	<tr><td>1775     </td><td>montana  </td><td> 5.976167</td></tr>
	<tr><td>1776     </td><td>montana  </td><td> 4.792833</td></tr>
	<tr><td>1777     </td><td>montana  </td><td> 4.821917</td></tr>
	<tr><td>1778     </td><td>montana  </td><td> 2.200900</td></tr>
	<tr><td>1779     </td><td>montana  </td><td> 5.074750</td></tr>
	<tr><td>1780     </td><td>montana  </td><td> 8.482500</td></tr>
	<tr><td>1781     </td><td>montana  </td><td> 2.654250</td></tr>
	<tr><td>1782     </td><td>montana  </td><td> 3.810143</td></tr>
	<tr><td>1796     </td><td>montana  </td><td> 9.157429</td></tr>
	<tr><td>1797     </td><td>montana  </td><td> 2.349833</td></tr>
	<tr><td>1811     </td><td>montana  </td><td> 4.370600</td></tr>
	<tr><td>1812     </td><td>montana  </td><td> 3.263250</td></tr>
	<tr><td>1813     </td><td>montana  </td><td> 6.526778</td></tr>
	<tr><td>1814     </td><td>montana  </td><td> 1.136667</td></tr>
	<tr><td>1815     </td><td>montana  </td><td> 3.830833</td></tr>
	<tr><td>1816     </td><td>montana  </td><td> 0.209375</td></tr>
	<tr><td>1817     </td><td>montana  </td><td> 3.638583</td></tr>
	<tr><td>1818     </td><td>montana  </td><td> 3.009000</td></tr>
	<tr><td>1819     </td><td>montana  </td><td>-7.710000</td></tr>
	<tr><td>1820     </td><td>montana  </td><td> 3.734250</td></tr>
	<tr><td>1821     </td><td>idaho    </td><td>-3.518000</td></tr>
	<tr><td>1821     </td><td>montana  </td><td> 4.536333</td></tr>
	<tr><td>1822     </td><td>idaho    </td><td>-0.483000</td></tr>
	<tr><td>1822     </td><td>montana  </td><td> 4.390833</td></tr>
	<tr><td>1823     </td><td>idaho    </td><td>-0.783875</td></tr>
	<tr><td>1823     </td><td>montana  </td><td> 4.038417</td></tr>
	<tr><td>1824     </td><td>idaho    </td><td> 4.201636</td></tr>
	<tr><td>...</td><td>...</td><td>...</td></tr>
	<tr><td>2006      </td><td>oregon    </td><td> 8.960333 </td></tr>
	<tr><td>2006      </td><td>washington</td><td> 8.675333 </td></tr>
	<tr><td>2007      </td><td>idaho     </td><td> 6.739417 </td></tr>
	<tr><td>2007      </td><td>montana   </td><td> 6.726917 </td></tr>
	<tr><td>2007      </td><td>oregon    </td><td> 8.895833 </td></tr>
	<tr><td>2007      </td><td>washington</td><td> 8.302417 </td></tr>
	<tr><td>2008      </td><td>idaho     </td><td> 5.509333 </td></tr>
	<tr><td>2008      </td><td>montana   </td><td> 5.283000 </td></tr>
	<tr><td>2008      </td><td>oregon    </td><td> 8.315167 </td></tr>
	<tr><td>2008      </td><td>washington</td><td> 7.685917 </td></tr>
	<tr><td>2009      </td><td>idaho     </td><td> 5.664250 </td></tr>
	<tr><td>2009      </td><td>montana   </td><td> 5.145417 </td></tr>
	<tr><td>2009      </td><td>oregon    </td><td> 8.559083 </td></tr>
	<tr><td>2009      </td><td>washington</td><td> 8.047583 </td></tr>
	<tr><td>2010      </td><td>idaho     </td><td> 5.920167 </td></tr>
	<tr><td>2010      </td><td>montana   </td><td> 5.426750 </td></tr>
	<tr><td>2010      </td><td>oregon    </td><td> 8.574583 </td></tr>
	<tr><td>2010      </td><td>washington</td><td> 8.442167 </td></tr>
	<tr><td>2011      </td><td>idaho     </td><td> 5.425667 </td></tr>
	<tr><td>2011      </td><td>montana   </td><td> 5.400167 </td></tr>
	<tr><td>2011      </td><td>oregon    </td><td> 8.077583 </td></tr>
	<tr><td>2011      </td><td>washington</td><td> 7.619333 </td></tr>
	<tr><td>2012      </td><td>idaho     </td><td> 6.955833 </td></tr>
	<tr><td>2012      </td><td>montana   </td><td> 6.998833 </td></tr>
	<tr><td>2012      </td><td>oregon    </td><td> 9.030083 </td></tr>
	<tr><td>2012      </td><td>washington</td><td> 8.477917 </td></tr>
	<tr><td>2013      </td><td>idaho     </td><td> 8.539222 </td></tr>
	<tr><td>2013      </td><td>montana   </td><td> 8.457556 </td></tr>
	<tr><td>2013      </td><td>oregon    </td><td>10.763556 </td></tr>
	<tr><td>2013      </td><td>washington</td><td>10.571778 </td></tr>
</tbody>
</table>


Finding the range and difference between max and min values for the entire duration of the dataset.

```R
pnw_data %>%
filter(region == 'washington') -> wa_data
print(range(wa_data$value))
print(max(wa_data$value) - min(wa_data$value))
```

    [1]  2.855625 10.571778
    [1] 7.716153
    


```R
pnw_data %>%
filter(region == 'oregon') -> or_data
print(range(or_data$value))
print(max(or_data$value) - min(or_data$value))
```

    [1]  3.46600 10.76356
    [1] 7.297556
    


```R
pnw_data %>%
filter(region == 'idaho') -> id_data
print(range(id_data$value))
print(max(id_data$value) - min(id_data$value))
```

    [1] -3.518000  8.539222
    [1] 12.05722
    


```R
pnw_data %>%
filter(region == 'montana') -> mo_data
print(range(mo_data$value))
print(max(mo_data$value) - min(mo_data$value))
```

    [1] -7.710000  9.157429
    [1] 16.86743
    
Plot the data to visualize the differences. Geom_smooth will smooth out some of the outliers. 

```R
ggplot(pnw_data)+
geom_smooth(aes(x=Year, y=value, col=region))+
labs(title='PNW Temperatures')+
ylab('Temperature in Celsius')

```

    `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    


    
![png](output_20_1.png)
    

Let's find more recent data and see how it compares. I am going to find the difference between the max and min values form 2000-2013. The warmest value for each state falls in 2013 so we can take the max - min to find the rate at which the state is warming. We will see that there is a low point for temps in 2008 and 2009.

```R
wa_data %>%
filter(Year >= 2000) -> latest_wa
print(max(latest_wa$value) - min(latest_wa$value))
print(latest_wa)
```

    [1] 2.952444
    [38;5;246m# A tibble: 14 x 3[39m
        Year region     value
       [3m[38;5;246m<int>[39m[23m [3m[38;5;246m<chr>[39m[23m      [3m[38;5;246m<dbl>[39m[23m
    [38;5;250m 1[39m  [4m2[24m000 washington  7.86
    [38;5;250m 2[39m  [4m2[24m001 washington  8.28
    [38;5;250m 3[39m  [4m2[24m002 washington  8.29
    [38;5;250m 4[39m  [4m2[24m003 washington  9.03
    [38;5;250m 5[39m  [4m2[24m004 washington  9.11
    [38;5;250m 6[39m  [4m2[24m005 washington  8.53
    [38;5;250m 7[39m  [4m2[24m006 washington  8.68
    [38;5;250m 8[39m  [4m2[24m007 washington  8.30
    [38;5;250m 9[39m  [4m2[24m008 washington  7.69
    [38;5;250m10[39m  [4m2[24m009 washington  8.05
    [38;5;250m11[39m  [4m2[24m010 washington  8.44
    [38;5;250m12[39m  [4m2[24m011 washington  7.62
    [38;5;250m13[39m  [4m2[24m012 washington  8.48
    [38;5;250m14[39m  [4m2[24m013 washington 10.6 
    


```R
or_data %>%
filter(Year >= 2000) -> latest_or
print(max(latest_or$value) - min(latest_or$value))
print(latest_or)
```

    [1] 2.685972
    [38;5;246m# A tibble: 14 x 3[39m
        Year region value
       [3m[38;5;246m<int>[39m[23m [3m[38;5;246m<chr>[39m[23m  [3m[38;5;246m<dbl>[39m[23m
    [38;5;250m 1[39m  [4m2[24m000 oregon  8.68
    [38;5;250m 2[39m  [4m2[24m001 oregon  8.88
    [38;5;250m 3[39m  [4m2[24m002 oregon  8.81
    [38;5;250m 4[39m  [4m2[24m003 oregon  9.56
    [38;5;250m 5[39m  [4m2[24m004 oregon  9.29
    [38;5;250m 6[39m  [4m2[24m005 oregon  8.84
    [38;5;250m 7[39m  [4m2[24m006 oregon  8.96
    [38;5;250m 8[39m  [4m2[24m007 oregon  8.90
    [38;5;250m 9[39m  [4m2[24m008 oregon  8.32
    [38;5;250m10[39m  [4m2[24m009 oregon  8.56
    [38;5;250m11[39m  [4m2[24m010 oregon  8.57
    [38;5;250m12[39m  [4m2[24m011 oregon  8.08
    [38;5;250m13[39m  [4m2[24m012 oregon  9.03
    [38;5;250m14[39m  [4m2[24m013 oregon 10.8 
    


```R
id_data %>%
filter(Year >= 2000) -> latest_id
print(max(latest_id$value) - min(latest_id$value))
print(latest_id)
```

    [1] 3.113556
    [38;5;246m# A tibble: 14 x 3[39m
        Year region value
       [3m[38;5;246m<int>[39m[23m [3m[38;5;246m<chr>[39m[23m  [3m[38;5;246m<dbl>[39m[23m
    [38;5;250m 1[39m  [4m2[24m000 idaho   6.24
    [38;5;250m 2[39m  [4m2[24m001 idaho   6.43
    [38;5;250m 3[39m  [4m2[24m002 idaho   5.78
    [38;5;250m 4[39m  [4m2[24m003 idaho   7.07
    [38;5;250m 5[39m  [4m2[24m004 idaho   6.39
    [38;5;250m 6[39m  [4m2[24m005 idaho   6.06
    [38;5;250m 7[39m  [4m2[24m006 idaho   6.43
    [38;5;250m 8[39m  [4m2[24m007 idaho   6.74
    [38;5;250m 9[39m  [4m2[24m008 idaho   5.51
    [38;5;250m10[39m  [4m2[24m009 idaho   5.66
    [38;5;250m11[39m  [4m2[24m010 idaho   5.92
    [38;5;250m12[39m  [4m2[24m011 idaho   5.43
    [38;5;250m13[39m  [4m2[24m012 idaho   6.96
    [38;5;250m14[39m  [4m2[24m013 idaho   8.54
    


```R
mo_data %>%
filter(Year >= 2000) -> latest_mo
print(max(latest_mo$value) - min(latest_mo$value))
print(latest_mo)
```

    [1] 3.312139
    [38;5;246m# A tibble: 14 x 3[39m
        Year region  value
       [3m[38;5;246m<int>[39m[23m [3m[38;5;246m<chr>[39m[23m   [3m[38;5;246m<dbl>[39m[23m
    [38;5;250m 1[39m  [4m2[24m000 montana  5.84
    [38;5;250m 2[39m  [4m2[24m001 montana  6.58
    [38;5;250m 3[39m  [4m2[24m002 montana  5.32
    [38;5;250m 4[39m  [4m2[24m003 montana  6.42
    [38;5;250m 5[39m  [4m2[24m004 montana  6.11
    [38;5;250m 6[39m  [4m2[24m005 montana  6.20
    [38;5;250m 7[39m  [4m2[24m006 montana  6.91
    [38;5;250m 8[39m  [4m2[24m007 montana  6.73
    [38;5;250m 9[39m  [4m2[24m008 montana  5.28
    [38;5;250m10[39m  [4m2[24m009 montana  5.15
    [38;5;250m11[39m  [4m2[24m010 montana  5.43
    [38;5;250m12[39m  [4m2[24m011 montana  5.40
    [38;5;250m13[39m  [4m2[24m012 montana  7.00
    [38;5;250m14[39m  [4m2[24m013 montana  8.46
    
Graph the Data. Montana and Idaho are cooler than Washington and Oregon (which is expected), however, the graph and value shows that Montana is warming the quickest of the 4 states.

```R
pnw_data %>%
filter(Year >= 2000) -> new_pnw

ggplot(new_pnw)+
geom_smooth(aes(x=Year, y=value, col=region))+
labs(title='PNW Temperatures')+
ylab('Temperature in Celsius')
```

    `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    


    
![png](output_25_1.png)
    

Put these values in a table for visualization. 

```R
slope <- c(2.95, 2.69, 3.11, 3.31)
state <- c('washington', 'oregon', 'idaho', 'montana')

df2 <- data.frame(state, slope)
df2
```


<table>
<thead><tr><th scope=col>state</th><th scope=col>slope</th></tr></thead>
<tbody>
	<tr><td>washington</td><td>2.95      </td></tr>
	<tr><td>oregon    </td><td>2.69      </td></tr>
	<tr><td>idaho     </td><td>3.11      </td></tr>
	<tr><td>montana   </td><td>3.31      </td></tr>
</tbody>
</table>


Box plot to see the difference more clearly.

```R
ggplot(data = df2, aes(x=state,y=slope,fill=state))+
geom_bar(stat='identity')
```


    
![png](output_27_0.png)
    

Now lets check how fast the PNW is warming compared to the US as a whole. 

Here we will grab US data for 2000-2013.

```R
clim_data %>%
filter(Year>=2000) %>%
group_by(Year) %>%
summarise(Temp = mean(AverageTemperature)) -> new_us
new_us
```


<table>
<thead><tr><th scope=col>Year</th><th scope=col>Temp</th></tr></thead>
<tbody>
	<tr><td>2000    </td><td>11.48388</td></tr>
	<tr><td>2001    </td><td>11.90173</td></tr>
	<tr><td>2002    </td><td>11.84943</td></tr>
	<tr><td>2003    </td><td>11.45334</td></tr>
	<tr><td>2004    </td><td>11.55826</td></tr>
	<tr><td>2005    </td><td>11.83835</td></tr>
	<tr><td>2006    </td><td>12.23848</td></tr>
	<tr><td>2007    </td><td>11.88225</td></tr>
	<tr><td>2008    </td><td>11.19365</td></tr>
	<tr><td>2009    </td><td>11.15633</td></tr>
	<tr><td>2010    </td><td>11.73288</td></tr>
	<tr><td>2011    </td><td>11.81109</td></tr>
	<tr><td>2012    </td><td>12.76184</td></tr>
	<tr><td>2013    </td><td>13.07763</td></tr>
</tbody>
</table>


Grabbing the same data as the above, but specifically for the PNW. 

```R
clim_data %>%
filter(Year>=2000) %>%
filter(State == 'Washington' | State == 'Oregon' | State == 'Montana' | 
       State == 'Idaho') %>%
group_by(Year) %>%
summarise(Temp = mean(AverageTemperature)) -> pnw2000
pnw2000
```


<table>
<thead><tr><th scope=col>Year</th><th scope=col>Temp</th></tr></thead>
<tbody>
	<tr><td>2000    </td><td>7.153187</td></tr>
	<tr><td>2001    </td><td>7.544250</td></tr>
	<tr><td>2002    </td><td>7.049000</td></tr>
	<tr><td>2003    </td><td>8.020542</td></tr>
	<tr><td>2004    </td><td>7.723063</td></tr>
	<tr><td>2005    </td><td>7.408500</td></tr>
	<tr><td>2006    </td><td>7.744833</td></tr>
	<tr><td>2007    </td><td>7.666146</td></tr>
	<tr><td>2008    </td><td>6.698354</td></tr>
	<tr><td>2009    </td><td>6.854083</td></tr>
	<tr><td>2010    </td><td>7.090917</td></tr>
	<tr><td>2011    </td><td>6.630687</td></tr>
	<tr><td>2012    </td><td>7.865667</td></tr>
	<tr><td>2013    </td><td>9.583028</td></tr>
</tbody>
</table>

Graph it.

```R
ggplot()+
geom_smooth(data=new_us, aes(x=Year, y=Temp), color='red')+
geom_smooth(data=pnw2000, aes(x=Year, y=Temp), color='blue')
```

    `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    `geom_smooth()` using method = 'loess' and formula 'y ~ x'
    


    
![png](output_30_1.png)
    

Find the difference between the max and min values. Again, we see the highest value in 2013 and a low point around 2008-2009. 

```R
print(max(new_us$Temp) - min(new_us$Temp))
```

    [1] 1.921299
    


```R
print(max(pnw2000$Temp) - min(pnw2000$Temp))
```

    [1] 2.95234
    
Calculate percent difference. According to this data, the PNW is warming 53.7% faster than the US as a whole. To take this further, I would be interested in gather newer data to see if this trend stays. 

```R
per_diff <- (abs(1.921299 - 2.95234)/1.921299) * 100
print(per_diff)
```

    [1] 53.66375
    
