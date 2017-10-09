:- use_module(regions,[]).
:- use_module(provinces,[]).
:- use_module(tourist_attractions,[]).
:- use_module(categories,[]).
:- use_module(activities,[]).
:- use_module(seasons,[]).

weather_from_temperature(Temp,Weather) :-
  (Temp<25
    -> Weather=cold
  ; Temp<29
    -> Weather=moderate
  ;
    Weather=hot
  ).

suggest_from_activity_id(ActivityID,TouristAttraction,Province,Region,Category) :-
  activities:activity_in_category(ActivityID,CategoryID),
  tourist_attractions:tourist_attraction_in_category(TouristAttractionID,CategoryID),
  tourist_attractions:tourist_attraction_in_province(TouristAttractionID,ProvinceID),
  provinces:province_in_region(ProvinceID,RegionID),
  tourist_attractions:tourist_attraction_name(TouristAttractionID,TouristAttraction),
  provinces:province_name(ProvinceID,Province),
  regions:region_name(RegionID,Region),
  categories:category_name(CategoryID,Category).

suggest_from_activity(Activity,TouristAttraction,Province,Region,Category) :-
  activities:activity_name(ActivityID,Activity),
  suggest_from_activity_id(ActivityID,TouristAttraction,Province,Region,Category).

suggest_from_age(MinAge,MaxAge,TouristAttraction,Province,Region,Category) :-
  activities:activity_min_age(ActivityID,Min),
  activities:activity_max_age(ActivityID,Max),
  abs(MinAge-Min)<5,
  abs(MaxAge-Max)<5,
  suggest_from_activity_id(ActivityID,TouristAttraction,Province,Region,Category).

suggest_from_min_age(MinAge,TouristAttraction,Province,Region,Category,MaxAge) :-
  activities:activity_min_age(ActivityID,Min),
  activities:activity_max_age(ActivityID,MaxAge),
  abs(MinAge-Min)<5,
  suggest_from_activity_id(ActivityID,TouristAttraction,Province,Region,Category).

suggest_from_max_age(MaxAge,TouristAttraction,Province,Region,Category,MinAge) :-
  activities:activity_min_age(ActivityID,MinAge),
  activities:activity_max_age(ActivityID,Max),
  abs(MaxAge-Max)<5,
  suggest_from_activity_id(ActivityID,TouristAttraction,Province,Region,Category).

suggest_from_cost(MinCost,MaxCost,TouristAttraction,Province,Region,Category) :-
  activities:activity_min_cost(ActivityID,Min),
  activities:activity_max_cost(ActivityID,Max),
  abs(MinCost-Min)<1000,
  abs(MaxCost-Max)<1000,
  suggest_from_activity_id(ActivityID,TouristAttraction,Province,Region,Category).

suggest_from_min_cost(MinCost,TouristAttraction,Province,Region,Category,MaxCost) :-
  activities:activity_min_cost(ActivityID,Min),
  activities:activity_max_cost(ActivityID,MaxCost),
  abs(MinCost-Min)<1000,
  suggest_from_activity_id(ActivityID,TouristAttraction,Province,Region,Category).

suggest_from_max_cost(MaxCost,TouristAttraction,Province,Region,Category,MinCost) :-
  activities:activity_min_cost(ActivityID,MinCost),
  activities:activity_max_cost(ActivityID,Max),
  abs(MaxCost-Max)<1000,
  suggest_from_activity_id(ActivityID,TouristAttraction,Province,Region,Category).

suggest_from_weather(Weather,TouristAttraction,Province,Region,Season,Category) :-
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
  categories:category_name(CategoryID,Category).
