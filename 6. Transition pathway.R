#' ---
#' title: "Graphs & tables"
#' author: "Frédéric Baudron"
#' date: "April 9th, 2024"
#' ---


rm(list=ls())


if (!require('nomnoml')) install.packages("nomnoml")

library(nomnoml)


# simple diagram
# see https://nomnoml.com/ for all available class diagram types and association types

nomnoml::nomnoml("

[<class>A] -> [<class>E]
[<class>A] -> [<class>D]

[<class>B] -> [<class>D]
[<class>B] -> [<class>E]
[<class>B] -> [<class>F]

[<class>C] -> [<class>E]

")


# add direction and change from "right" to "bottom" to see what happens
# add gravity and play with different numbers to see what happens
# add fill and play with different colors (e.g. "red" or other HEX color code) to see what happens


nomnoml::nomnoml("
#direction: right
#gravity: 3
#fill: #ffffff


[<class>A] -> [<class>E]
[<class>A] -> [<class>D]

[<class>B] -> [<class>D]
[<class>B] -> [<class>E]
[<class>B] -> [<class>F]

[<class>C] -> [<class>E]

")


# draw inside a frame
# change the 2 colors in "fill" and see what happens

nomnoml::nomnoml("
#direction: right
#gravity: 3
#fill: #ffffff;#ffffff;


[<frame>Title|


[<class>A] -> [<class>E]
[<class>A] -> [<class>D]

[<class>B] -> [<class>D]
[<class>B] -> [<class>E]
[<class>B] -> [<class>F]

[<class>C] -> [<class>E]

]

")


# changing text: make sure they totally match when appearing at different instances or separate boxes will be drawn

nomnoml::nomnoml("
#direction: right
#gravity: 3
#fill: #ffffff;#ffffff;


[<frame>AE-I Zimbabwe Transition Pathway|


[<class>Better informed policy discussions; and regulatory options leading;to policy-makers enacting policies;supportive of agroecology] -> [<class>Increased capacity to produce;agroecological goods for;sustained food and;nutrition security]
[<class>Better informed policy discussions; and regulatory options leading;to policy-makers enacting policies;supportive of agroecology] -> [<class>Better market prices;for commodities produced;agroecologically]

[<class>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products] -> [<class>Better market prices;for commodities produced;agroecologically]
[<class>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products] -> [<class>Increased capacity to produce;agroecological goods for;sustained food and;nutrition security]
[<class>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products] -> [<class>Better access to innovative,;locally and agroecologically;produced goods]

[<class>Effective and coordinated; implementation of agroecological solutions] -> [<class>Increased capacity to produce;agroecological goods for;sustained food and;nutrition security]

]

")


# add actors linked by dashed lines

nomnoml::nomnoml("
#direction: right
#gravity: 3
#fill: #ffffff;#ffffff;


[<frame>AE-I Zimbabwe Transition Pathway|


[<class>Better informed policy discussions; and regulatory options leading;to policy-makers enacting policies;supportive of agroecology] -> [<class>Increased capacity to produce;agroecological goods for;sustained food and;nutrition security]
[<class>Better informed policy discussions; and regulatory options leading;to policy-makers enacting policies;supportive of agroecology] -> [<class>Better market prices;for commodities produced;agroecologically]

[<class>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products] -> [<class>Better market prices;for commodities produced;agroecologically]
[<class>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products] -> [<class>Increased capacity to produce;agroecological goods for;sustained food and;nutrition security]
[<class>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products] -> [<class>Better access to innovative,;locally and agroecologically;produced goods]

[<class>Effective and coordinated; implementation of agroecological solutions] -> [<class>Increased capacity to produce;agroecological goods for;sustained food and;nutrition security]


[<class>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products] -- [<actor>Private sector]

[<class>Better market prices;for commodities produced;agroecologically] -- [<actor>Private sector]

[<class>Better informed policy discussions; and regulatory options leading;to policy-makers enacting policies;supportive of agroecology] -- [<actor>Policy-makers]

[<class>Increased capacity to produce;agroecological goods for;sustained food and;nutrition security] -- [<actor>Farmers]
[<class>Better market prices;for commodities produced;agroecologically] -- [<actor>Farmers]
[<class>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products]-- [<actor>Farmers]

[<class>Better access to innovative,;locally and agroecologically;produced goods] -- [<actor>Consumers]

[<class>Effective and coordinated; implementation of agroecological solutions] -- [<actor>Development NGOs]

]

")


# custom classifiers for outcome and impact

nomnoml::nomnoml("
#direction: right
#gravity: 3
#fill: #ffffff;#ffffff;


#.outcome: visual=class fill=#00bfc4
#.impact: visual=class fill=#c77cff


[<frame>AE-I Zimbabwe Transition Pathway|


[<outcome>Better informed policy discussions; and regulatory options leading;to policy-makers enacting policies;supportive of agroecology] -> [<impact>Increased capacity to produce;agroecological goods for;sustained food and;nutrition security]
[<outcome>Better informed policy discussions; and regulatory options leading;to policy-makers enacting policies;supportive of agroecology] -> [<impact>Better market prices;for commodities produced;agroecologically]

[<outcome>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products] -> [<impact>Better market prices;for commodities produced;agroecologically]
[<outcome>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products] -> [<impact>Increased capacity to produce;agroecological goods for;sustained food and;nutrition security]
[<outcome>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products] -> [<impact>Better access to innovative,;locally and agroecologically;produced goods]

[<outcome>Effective and coordinated; implementation of agroecological solutions] -> [<impact>Increased capacity to produce;agroecological goods for;sustained food and;nutrition security]


[<outcome>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products] -- [<actor>Private sector]

[<impact>Better market prices;for commodities produced;agroecologically] -- [<actor>Private sector]

[<outcome>Better informed policy discussions; and regulatory options leading;to policy-makers enacting policies;supportive of agroecology] -- [<actor>Policy-makers]

[<impact>Increased capacity to produce;agroecological goods for;sustained food and;nutrition security] -- [<actor>Farmers]
[<impact>Better market prices;for commodities produced;agroecologically] -- [<actor>Farmers]
[<outcome>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products]-- [<actor>Farmers]

[<impact>Better access to innovative,;locally and agroecologically;produced goods] -- [<actor>Consumers]

[<outcome>Effective and coordinated; implementation of agroecological solutions] -- [<actor>Development NGOs]

]

")


# Adding activities and outputs

nomnoml::nomnoml("
#direction: right
#gravity: 3
#fill: #ffffff;#ffffff;

#.activity: visual=class fill=#f8766d
#.output: visual=class fill=#7cae00
#.outcome: visual=class fill=#00bfc4
#.impact: visual=class fill=#c77cff


[<frame>AE-I Zimbabwe Transition Pathway|


[<activity>Engaging, communicating, and;integrating research and;indigenous knowledge] -> [<output>Agroecological innovations for;agricultural systems (practices);co-designed with farmers,;scientists, and extension;agents (considering food;security, safe production,;and dietary diversity aspects)]
[<activity>Engaging, communicating, and;integrating research and;indigenous knowledge] -> [<output>Coordinated and integrated;platforms for blended;training]

[<activity>Collecting baseline data;to establish indicators;to monitor transitions] -> [<output>Context-specific indicators;to monitor and understand;agroecology transitions]

[<activity>Advancing and supporting;existing business environment]->[<output>New or redesigned business;models co-developed under;agroecological principles]
    
[<activity>Understanding programs and;policies for implementing;and scaling more sustainable;and equitable agroecology;transition pathways]->[<output>Opportunities for improving; the potential of local institutions;and governance structures;to catalyze, agroecological;transitions identified with;food system actors in;each ALL]
[<activity>Understanding programs and;policies for implementing;and scaling more sustainable;and equitable agroecology;transition pathways]->[<output>Identified policies that favour,;limit, or impede agroecological;transitions and new opportunities;for policy integration]
[<activity>Understanding programs and;policies for implementing;and scaling more sustainable;and equitable agroecology;transition pathways]->[<output>Recommendations and action; plans for policy and institutional; changes in ALL countries or regions]
[<activity>Understanding social agency;and community behaviour] -> [<output>Key determinants and drivers;of agency and behavioural factors;that influence inclusive;agroecological transitions;are identified and incorporated;in strategies and investment plans]


[<output>Agroecological innovations for;agricultural systems(practices);co-designed with farmers,;scientists, and extension;agents (considering food;security, safe production,;and dietary diversity aspects)] -> [<outcome>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products]

[<output>Coordinated and integrated;platforms for blended;training] -> [<outcome>Effective and coordinated;implementation of agroecological;solutions]
   
[<output>Context-specific indicators;to monitor and understand;agroecology transitions] -> [<outcome>Effective and coordinated;implementation of agroecological;solutions]

[<output>New or redesigned business;models co-developed under;agroecological principles] -> [<outcome>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products]

[<output>Opportunities for improving; the potential of local institutions;and governance structures;to catalyze, agroecological;transitions identified with;food system actors in;each ALL]->[<outcome>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products]
[<output>Opportunities for improving; the potential of local institutions;and governance structures;to catalyze, agroecological;transitions identified with;food system actors in;each ALL] -> [<outcome>Better informed policy discussions; and regulatory options leading;to policy-makers enacting policies;supportive of agroecology]

[<output>Identified policies that favour,;limit, or impede agroecological;transitions and new opportunities;for policy integration] -> [<outcome>Better informed policy discussions; and regulatory options leading;to policy-makers enacting policies;supportive of agroecology]
[<output>Identified policies that favour,;limit, or impede agroecological;transitions and new opportunities;for policy integration] -> [<outcome>Better informed policy discussions; and regulatory options leading;to policy-makers enacting policies;supportive of agroecology]

[<output>Recommendations and action; plans for policy and institutional; changes in ALL countries or regions] -> [<outcome>Better informed policy discussions; and regulatory options leading;to policy-makers enacting policies;supportive of agroecology]

[<output>Key determinants and drivers;of agency and behavioural factors;that influence inclusive;agroecological transitions;are identified and incorporated;in strategies and investment plans] -> [<outcome>Better informed policy discussions; and regulatory options leading;to policy-makers enacting policies;supportive of agroecology]
[<output>Key determinants and drivers;of agency and behavioural factors;that influence inclusive;agroecological transitions;are identified and incorporated;in strategies and investment plans] -> [<outcome>Effective and coordinated;implementation of agroecological;solutions]


[<outcome>Better informed policy discussions; and regulatory options leading;to policy-makers enacting policies;supportive of agroecology] -> [<impact>Increased capacity to produce;agroecological goods for;sustained food and;nutrition security]
[<outcome>Better informed policy discussions; and regulatory options leading;to policy-makers enacting policies;supportive of agroecology] -> [<impact>Better market prices;for commodities produced;agroecologically]

[<outcome>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products] -> [<impact>Better market prices;for commodities produced;agroecologically]
[<outcome>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products] -> [<impact>Increased capacity to produce;agroecological goods for;sustained food and;nutrition security]
[<outcome>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products] -> [<impact>Better access to innovative,;locally and agroecologically;produced goods]

[<outcome>Effective and coordinated; implementation of agroecological solutions] -> [<impact>Increased capacity to produce;agroecological goods for;sustained food and;nutrition security]


[<outcome>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products] -- [<actor>Private sector]

[<impact>Better market prices;for commodities produced;agroecologically] -- [<actor>Private sector]

[<outcome>Better informed policy discussions; and regulatory options leading;to policy-makers enacting policies;supportive of agroecology] -- [<actor>Policy-makers]

[<impact>Increased capacity to produce;agroecological goods for;sustained food and;nutrition security] -- [<actor>Farmers]
[<impact>Better market prices;for commodities produced;agroecologically] -- [<actor>Farmers]
[<outcome>Increased and improved capacity of;value chain actors to forecast demand;and deliver quality products]-- [<actor>Farmers]

[<impact>Better access to innovative,;locally and agroecologically;produced goods] -- [<actor>Consumers]

[<outcome>Effective and coordinated; implementation of agroecological solutions] -- [<actor>Development NGOs]

]

")


