# Critical Path Method

## The critical path method, or critical path analysis, is an algorithm for scheduling a set of project activities. It is commonly used in conjunction with the program evaluation and review technique.

This is a R program that
 -   takes an activity table with each activity's titel, immediate predecessor(s) and time and stores it into a graph
 -   using a modified Breadth First Search algorithm to traverse the graph to find the forward and backward path
 -   in the forward path it calculates the ES (Early Start time) and EF (Early Finish time ) of each activity
 -   in the backward path it calculates the LS (Late Start time ) and LF (Late Finish time ) of each activity
 -   calculate the slack value of each activity 

        ES (Early Start Time)  = maximum value of "EF" for all the activities it depends on  
        EF (Early Finish Time) = ES + Activity time  
        LF (Late Finish Time) = minimum value of "LS" for all the activites that depends on this activity 
        LS (Late Start Time ) = LF - Activity time  
        S (Slack Value) = LS - ES of each activity, if it's zero then this activity is in the critical path 
 -   draws a plot showing the graph, the attributes for each node and the critical path (green nodes),

### Here is Some Sample Tests And It's Output

| Activity | Title                | Immediate Predecessors | Activity Time |
| -------- | -------------------- | ------------ | -------- |
| A        | Identify market need |       -      | 3        |
| B        | Conduct R&D          |A        | 60       |
| C        | Design packaging     |A            | 5        |
| D        | Select product size  |A            | 15       |
| E        | Conduct product test |B            | 6        |
| F        | Install production process |D            | 40       |
| G        | Perform market analysis |C E          | 10       |
| H        | Production startup |F            | 7        |
| I        | Make modifications |G            | 6        |
| J        | Market product |H I          | 12       |

## ![fig1](https://raw.githubusercontent.com/cppcoders/critical-path-method-R/master/images/data/directed%20tree.png)

## ![fig1](https://raw.githubusercontent.com/cppcoders/critical-path-method-R/master/images/data/info%20graph.png)

&nbsp;
&nbsp;
<hr/>
&nbsp;
&nbsp;

|Activity |	Title	| Immediate Predecessor	| Activity Time
|-------- | ----- | -------------------- | --------------- |
|A	|High-level analysis|	-|	7||
|B	|Selection of hardware platform|	A | 	7|
|C	|Installation and commissioning of hardware|	B|	14|
|D	|Detailed analysis of core modules|	A |	14|
|E	|Detailed analysis of supporting modules|	D	|14|
|F	|Programming of core modules|	D|	14|
|G	|Programming of supporting modules|	E|	21|
|H	|Quality assurance of core modules|	F|	7|
|I	|Quality assurance of supporting modules|	G|	7|
|J	|Core module training	|C H|	1|
|K	|Development and QA of accounting reporting|	E|	7|
|L	|Development and QA of management reporting|	E|	7|
|M	|Development of Management Information System|	L|	7|
|N	|Detailed training|	I J K M	| 7|

## ![fig1](https://raw.githubusercontent.com/cppcoders/critical-path-method-R/master/images/test1/directed%20tree.png)

## ![fig1](https://raw.githubusercontent.com/cppcoders/critical-path-method-R/master/images/test1/info%20graph.png)

&nbsp;
&nbsp;
<hr/>
&nbsp;
&nbsp;

| Activity |Title | Immeediate Predecessors | Activity Time |
| -------- |----- |------------ | -------- |
| A        | "TEST" | -            | 2        |
| B        | "TSET" | A            | 4        |
| C        | "TEST" |  B            | 3        |
| D        | "TEST" |  B            | 2        |
| E        | "TEST" | C D          | 10       |
| F        | "TEST" | B            | 4        |

## ![fig1](https://raw.githubusercontent.com/cppcoders/critical-path-method-R/master/images/test2/directed%20tree.png)

## ![fig1](https://raw.githubusercontent.com/cppcoders/critical-path-method-R/master/images/test2/ingo%20graph.png)

&nbsp;
&nbsp;
<hr/>
&nbsp;
&nbsp;

| Activity |Title | Immediate Predecessors | Activity Time |
| -------- |------| ------------ | -------- |
| A        | "TEST"|-            | 3        |
| B        | "TEST"|A            | 60       |
| C        | "TEST"|A            | 5        |
| D        | "TEST"|A            | 15       |
| E        | "TEST"|B            | 6        |
| F        | "TEST"|D            | 40       |
| G        | "TEST"|C E          | 10       |
| H        | "TEST"|F            | 7        |


## ![fig1](https://raw.githubusercontent.com/cppcoders/critical-path-method-R/master/images/test3/directed%20tree.png)

## ![fig1](https://raw.githubusercontent.com/cppcoders/critical-path-method-R/master/images/test3/info%20graph.png)



