:- module(rules,[]).

suggest_from_activity_id(ActivityID,TouristAttraction,Province,Region,Category,Rating) :-
  activities:activity_in_category(ActivityID,CategoryID),
  tourist_attractions:tourist_attraction_in_category(TouristAttractionID,CategoryID),
  tourist_attractions:tourist_attraction_in_province(TouristAttractionID,ProvinceID),
  provinces:province_in_region(ProvinceID,RegionID),
  tourist_attractions:tourist_attraction_name(TouristAttractionID,TouristAttraction),
  provinces:province_name(ProvinceID,Province),
  regions:region_name(RegionID,Region),
  categories:category_name(CategoryID,Category),
  tourist_attractions:tourist_attraction_rating(TouristAttractionID,Rating).

suggest_from_tourist_attraction(TouristAttraction,Province,Region,Category,Rating) :-
  tourist_attractions:tourist_attraction_in_category(TouristAttractionID,CategoryID),
  tourist_attractions:tourist_attraction_in_province(TouristAttractionID,ProvinceID),
  provinces:province_in_region(ProvinceID,RegionID),
  tourist_attractions:tourist_attraction_name(TouristAttractionID,TouristAttraction),
  provinces:province_name(ProvinceID,Province),
  regions:region_name(RegionID,Region),
  categories:category_name(CategoryID,Category),
  tourist_attractions:tourist_attraction_rating(TouristAttractionID,Rating).

suggest_from_activity(Activity,TouristAttraction,Province,Region,Category,Rating) :-
  activities:activity_name(ActivityID,Activity),
  suggest_from_activity_id(ActivityID,TouristAttraction,Province,Region,Category,Rating).

time_from_opening_time(OpeningTime,Time) :-
  (OpeningTime=<5.00 ->
    Time='Early Morning'
  ; OpeningTime=<10.00 ->
    Time='Morning'
  ; OpeningTime=<13.00 ->
    Time='Noon'
  ; OpeningTime=<16.00 ->
    Time='Afternoon'
  ; OpeningTime=<19.00 ->
    Time='Evening'
  ;
    Time='Night'
  ).

suggest_from_opening_hours(Time,TouristAttraction,Province,Region,Category,Rating,OpeningTimeFormatted,ClosingTimeFormatted) :-
  tourist_attractions:tourist_attraction_name(_,TouristAttraction),
  tourist_attractions:tourist_attraction_opening_time(_,OpeningTime),
  time_from_opening_time(OpeningTime,Time),
  suggest_from_opening_time(OpeningTime,TouristAttraction,Province,Region,Category,Rating,OpeningTimeFormatted,ClosingTimeFormatted).

suggest_from_opening_and_closing_time(OpeningTime,ClosingTime,TouristAttraction,Province,Region,Category,Rating,OpeningTimeFormatted,ClosingTimeFormatted) :-
  tourist_attractions:tourist_attraction_name(TouristAttractionID,TouristAttraction),
  tourist_attractions:tourist_attraction_opening_time(TouristAttractionID,Opening),
  tourist_attractions:tourist_attraction_closing_time(TouristAttractionID,Closing),
  abs(OpeningTime-Opening)=<1,
  abs(ClosingTime-Closing)=<1,
  format(atom(OpeningTimeFormatted),'~2f',[OpeningTime]),
  format(atom(ClosingTimeFormatted),'~2f',[ClosingTime]),
  suggest_from_tourist_attraction(TouristAttraction,Province,Region,Category,Rating).

suggest_from_opening_time(OpeningTime,TouristAttraction,Province,Region,Category,Rating,OpeningTimeFormatted,ClosingTimeFormatted) :-
  tourist_attractions:tourist_attraction_name(TouristAttractionID,TouristAttraction),
  tourist_attractions:tourist_attraction_opening_time(TouristAttractionID,Opening),
  tourist_attractions:tourist_attraction_closing_time(TouristAttractionID,ClosingTime),
  abs(OpeningTime-Opening)=<1,
  format(atom(OpeningTimeFormatted),'~2f',[Opening]),
  format(atom(ClosingTimeFormatted),'~2f',[ClosingTime]),
  suggest_from_tourist_attraction(TouristAttraction,Province,Region,Category,Rating).

suggest_from_closing_time(ClosingTime,TouristAttraction,Province,Region,Category,Rating,OpeningTimeFormatted,ClosingTimeFormatted) :-
  tourist_attractions:tourist_attraction_name(TouristAttractionID,TouristAttraction),
  tourist_attractions:tourist_attraction_opening_time(TouristAttractionID,OpeningTime),
  tourist_attractions:tourist_attraction_closing_time(TouristAttractionID,Closing),
  abs(ClosingTime-Closing)=<1,
  format(atom(OpeningTimeFormatted),'~2f',[OpeningTime]),
  format(atom(ClosingTimeFormatted),'~2f',[Closing]),
  suggest_from_tourist_attraction(TouristAttraction,Province,Region,Category,Rating).

suggest_from_opening_now(TouristAttraction,Province,Region,Category,Rating,OpeningTimeFormatted,ClosingTimeFormatted) :-
  tourist_attractions:tourist_attraction_name(TouristAttractionID,TouristAttraction),
  tourist_attractions:tourist_attraction_opening_time(TouristAttractionID,OpeningTime),
  tourist_attractions:tourist_attraction_closing_time(TouristAttractionID,ClosingTime),
  get_time(Time),
  stamp_date_time(Time,date(_,_,_,H,_,_,_,_,_),'UTC'),
  Hour is H+7,
  Hour>OpeningTime,
  Hour<ClosingTime,
  format(atom(OpeningTimeFormatted),'~2f',[OpeningTime]),
  format(atom(ClosingTimeFormatted),'~2f',[ClosingTime]),
  suggest_from_tourist_attraction(TouristAttraction,Province,Region,Category,Rating).

age_group_from_age(Age,AgeGroup) :-
  (Age<13 ->
    AgeGroup='Kids'
  ; Age<20 ->
    AgeGroup='Teenagers'
  ; Age<25 ->
    AgeGroup='Young Adults'
  ; Age<60 ->
    AgeGroup='Middle Aged'
  ;
    AgeGroup='Elders'
  ).

suggest_from_age(AgeGroup,TouristAttraction,Province,Region,Category,Rating,MinAge,MaxAge) :-
  activities:activity_min_age(_,MinAge),
  age_group_from_age(MinAge,AgeGroup),
  suggest_from_min_age(MinAge,TouristAttraction,Province,Region,Category,Rating,MaxAge).

suggest_from_age(MinAge,MaxAge,TouristAttraction,Province,Region,Category,Rating) :-
  activities:activity_min_age(ActivityID,Min),
  activities:activity_max_age(ActivityID,Max),
  abs(MinAge-Min)=<5,
  abs(MaxAge-Max)=<5,
  suggest_from_activity_id(ActivityID,TouristAttraction,Province,Region,Category,Rating).

suggest_from_min_age(MinAge,TouristAttraction,Province,Region,Category,Rating,MaxAge) :-
  activities:activity_min_age(ActivityID,Min),
  activities:activity_max_age(ActivityID,MaxAge),
  abs(MinAge-Min)=<5,
  suggest_from_activity_id(ActivityID,TouristAttraction,Province,Region,Category,Rating).

suggest_from_max_age(MaxAge,TouristAttraction,Province,Region,Category,Rating,MinAge) :-
  activities:activity_min_age(ActivityID,MinAge),
  activities:activity_max_age(ActivityID,Max),
  abs(MaxAge-Max)=<5,
  suggest_from_activity_id(ActivityID,TouristAttraction,Province,Region,Category,Rating).

expensiveness_from_cost(Cost,Expensiveness) :-
  (Cost<500 ->
    Expensiveness='Cheap'
  ; Cost<2500 ->
    Expensiveness='Affordable'
  ;
    Expensiveness='Expensive'
  ).

suggest_from_cost(Expensiveness,TouristAttraction,Province,Region,Category,Rating,MinCost,MaxCost) :-
  activities:activity_min_cost(_,MinCost),
  expensiveness_from_cost(MinCost,Expensiveness),
  suggest_from_min_cost(MinCost,TouristAttraction,Province,Region,Category,Rating,MaxCost).

suggest_from_cost(MinCost,MaxCost,TouristAttraction,Province,Region,Category,Rating) :-
  activities:activity_min_cost(ActivityID,Min),
  activities:activity_max_cost(ActivityID,Max),
  abs(MinCost-Min)=<1000,
  abs(MaxCost-Max)=<1000,
  suggest_from_activity_id(ActivityID,TouristAttraction,Province,Region,Category,Rating).

suggest_from_min_cost(MinCost,TouristAttraction,Province,Region,Category,Rating,MaxCost) :-
  activities:activity_min_cost(ActivityID,Min),
  activities:activity_max_cost(ActivityID,MaxCost),
  abs(MinCost-Min)=<1000,
  suggest_from_activity_id(ActivityID,TouristAttraction,Province,Region,Category,Rating).

suggest_from_max_cost(MaxCost,TouristAttraction,Province,Region,Category,Rating,MinCost) :-
  activities:activity_min_cost(ActivityID,MinCost),
  activities:activity_max_cost(ActivityID,Max),
  abs(MaxCost-Max)=<1000,
  suggest_from_activity_id(ActivityID,TouristAttraction,Province,Region,Category,Rating).

weather_from_temperature(Temp,Weather) :-
  (Temp<25 ->
    Weather='Cold'
  ; Temp<29 ->
    Weather='Moderate'
  ;
    Weather='Hot'
  ).

suggest_from_weather(Weather,TouristAttraction,Province,Region,Season,AvgTemp,Category,Rating) :-
  seasons:season_average_temp(SeasonID,SeasonAvgTemp),
  weather_from_temperature(SeasonAvgTemp,Weather),
  seasons:season_in_region(SeasonID,RegionID),
  provinces:province_in_region(ProvinceID,RegionID),
  tourist_attractions:tourist_attraction_in_province(TouristAttractionID,ProvinceID),
  tourist_attractions:tourist_attraction_in_category(TouristAttractionID,CategoryID),
  tourist_attractions:tourist_attraction_name(TouristAttractionID,TouristAttraction),
  provinces:province_name(ProvinceID,Province),
  regions:region_name(RegionID,Region),
  seasons:season_name(SeasonID,Season),
  seasons:season_average_temp(SeasonID,AvgTemp),
  categories:category_name(CategoryID,Category),
  tourist_attractions:tourist_attraction_rating(TouristAttractionID,Rating).
