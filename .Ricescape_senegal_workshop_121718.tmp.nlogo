extensions [ gis ]

breed [ roads road ]

roads-own [
  my-patches
  my-length
  my-paved
  my-min-start-distance
  my-avoided-flood-sum
  my-avoided-flood-proportion
  my-max-storage
  my-max-villages
  my-criteria-sum
  my-roads-ID
]

globals [
  farms-dataset
  roadsID-dataset
  roadsPaved-dataset
  storageCapacity-dataset
  farm-probability-dataset
  hand-dataset
  excluded-classes-dataset
  initialPavedRatio
  networkSpeed
  walking-distance
  world_raster
  simulation_complete
  current-roads-budget
  roads-budget
  population-next
  population-current
  population-growth
  additional-people
  crop-quantity
  remaining-roads-budget
  villages-along-paved
  total-storage-added
  crops-per-person
  initial-farm-count
]

patches-own [
  farm
  roadsID
  roadsPaved
  initialStorage
  storageCapacity
  farmProbability
  capacityDifference
  roadStartDistance
  roadEndDistance
  checkSilo
  silosAlongRoads
  villagesAlongRoads
  normalizedSilosAlongRoads
  normalizedVillagesAlongRoads
  normalizedAvoidedFloodProbability
  elevation
  avoidedFloodProbability
  villageDistance
  dummyFarmProbabilitySum
  eligibleVillageDistance
  dummyFarmProbability
  eligibleVillagePatch?
  excludedClasses
;  roadLength
  my-road
  nearOtherVillage?
  villageConnected?
  farmConnected?
  farmCounted?
  eligibleVillagePatchNearRoad?
]

turtles-own [
  distanceTurtle?
  homeDistance
  homeSilos
  homeVillages
]

to setup
  clear-all
  recalculate-variables
  setup-gis
  display-excluded-classes
  apply-roadsID
  display-roadsPaved
  display-storageCapacity
  display-road-flood-risk
  display-farms
  calculate-crop-quantity
  calculate-crops-per-person
  compute-manhattan-distances-out
  compute-manhattan-distances-back-setup
  calculate-village-distance
  calculate-initial-paved-ratio
  calculate-road-length
  create-population
  check-villages-connected
  check-farms-connected
  reset-ticks
  set simulation_complete false
end

to go
 ; export-world-raster
  show-year
  recalculate-variables
  grow-population
  calculate-road-flood-risk
  calculate-village-distance
  calculate-farmProbability
  expand-farms
  calculate-crop-quantity
  if (add-new-villages = true) [
    add-villages
  ]
  if (add-storage = true) [
    add-storageCapacity
  ]
  compute-manhattan-distances-back-go
  normalize-criteria-values
  pave-roads
  ; including the below procedure in the go procedure
  ; is necessary for new paved roads to mostly connect
  ; with existing paved roads as in the setup
  compute-manhattan-distances-out
  calculate-network-speed
  count-villages-along-paved
  calculate-total-storage-added-each-tick
  check-villages-connected
  check-farms-connected
  tick
  if (simulation_complete = true) [ stop ]
end

to recalculate-variables
  set walking-distance round travel-distance * cells-per-km
  ; each km of road costs 25 million CFA to build
  ; multiply by 1 km / 25 million CFA to get km
  ; then multiply by 4 grid cells / km to get budget
  ; in grid cell length
  set roads-budget round roads-investment * 1 / 25 * cells-per-km
  set population-growth population-growth-rate / 100
end

to setup-gis
  set-current-directory file-path; Setting working directory. Lasse/Adam should fill in "/home/lassegs/dev/darwin-models/" here
  set farms-dataset gis:load-dataset (word community "_data/" community "_EO_cropland.asc")
  set roadsID-dataset gis:load-dataset (word community "_data/" community "_roads_ID.asc")
  set roadsPaved-dataset gis:load-dataset (word community "_data/" community "_roads_paved.asc")
  set storageCapacity-dataset gis:load-dataset (word community "_data/storage_" community "_capacity.asc")
  set farm-probability-dataset gis:load-dataset (word community "_data/" community "_farm_probability.asc")
  set hand-dataset gis:load-dataset (word community "_data/" community "_hand.asc")
  ifelse (community = "bandafassi") [
    set excluded-classes-dataset gis:load-dataset (word community "_data/" community "_EO_trees.asc")
  ] [
    set excluded-classes-dataset gis:load-dataset (word community "_data/" community "_excluded_classes.asc")
  ]
end

to display-excluded-classes
  gis:apply-raster excluded-classes-dataset excludedClasses
  ask patches [
    ifelse (excludedClasses > 1)
    [ set excludedClasses excludedClasses ]
    [ set excludedClasses -1 ]
  ]
end

to display-road-flood-risk
  gis:apply-raster hand-dataset elevation
  ask patches with [ roadsPaved = 0 and storageCapacity = -1 ] [
    if (elevation < flood-risk-elevation )
    [ set pcolor blue ]
  ]
end

to display-farms
  gis:apply-raster farms-dataset farm
  ask patches [
    ifelse (farm = 1)
    [ set farm farm ]
    [ set farm -1 ]
  ]
  ask patches with [ farm > 0 and roadsID = -2 and storageCapacity = -1] [
    if ( elevation < irrigated-elevation ) [
      set pcolor green
    ]
    if ( elevation > irrigated-elevation ) [
      set pcolor lime
    ]
  ]
  set initial-farm-count count patches with [ farm > 0 ]
end

to apply-roadsID
  gis:apply-raster roadsID-dataset roadsID
  ask patches [
    ifelse (roadsID > -2)
    [ set roadsID roadsID
      set roadStartDistance 999999
      set roadEndDistance 999999 ]
    [ set roadsID -2 ]
  ]
end

to display-roadsPaved
  gis:apply-raster roadsPaved-dataset roadsPaved
  ask patches [
    if (roadsPaved > 0)
    [ set pcolor gray ]
    if (roadsPaved = 0)
    [ set pcolor brown ]
  ]
end

to display-storageCapacity
  gis:apply-raster storageCapacity-dataset storageCapacity
  ask patches [
    ifelse (storageCapacity >= 0)
    [ set pcolor red ]
    [ set storageCapacity -1 ]
  ]
  gis:apply-raster storageCapacity-dataset initialStorage
  ask patches [
    ifelse (initialStorage >= 0)
    [ set initialStorage initialStorage ]
    [ set initialStorage -1 ]
  ]
end

to calculate-village-distance
  ask patches with [ storageCapacity >= 0 ] [
    ask patches in-radius walking-distance [
      set villageDistance distance myself
    ]
  ]
end

to calculate-initial-paved-ratio
  set networkSpeed 17.21
  let totalRoads count patches with [ roadsID > -2 ]
  let pavedRoads count patches with [ roadsID > -2 and roadsPaved = 1 ]
  set initialPavedRatio pavedRoads / totalRoads
end

to calculate-road-length
  let roadsIDList [ roadsID ] of patches with [ roadsPaved >= 0 ]
  let roadsIDListUnique remove-duplicates roadsIDList

  let max-roadsID max [ roadsID ] of patches with [ roadsPaved >= 0 ]

  create-roads (max-roadsID + 1)

  let road_indices [ who ] of roads

  let first_road_index item 1 sort road_indices

  let i first_road_index

  foreach roadsIDListUnique [ x ->
    ask patches with [ roadsID = x ] [
      set my-road [ who ] of road i
    ]
    set i i + 1
  ]

  ask roads [
    set my-patches patches with [ my-road = [who] of myself ]
    set my-length count my-patches
    if my-length > 0 [
      ifelse min [ roadsPaved ] of my-patches > 0 [
        set my-paved 1
      ] [
        set my-paved 0
      ]
      set my-roads-ID max [ roadsID ] of my-patches
    ]
    hide-turtle
  ]
end

to create-population
  set population-current initial-population
end

to calculate-road-flood-risk
  ask patches with [ roadsID > -2 ] [
    ifelse ( elevation < flood-risk-elevation ) [
      set avoidedFloodProbability 1 - ( ( flood-risk-elevation - elevation ) / flood-risk-elevation )
    ] [
      set avoidedFloodProbability 1
    ]
  ]
end

to calculate-farmProbability
  ask patches with [ excludedClasses = -1 and villageDistance > 0 and farm = -1 and roadsID = -2 and storageCapacity = -1 ] [
    set farmProbability 1 - ( villageDistance / walking-distance ) + 0.001
  ]
end

to check-elevation-and-expand-farms
  if ( elevation < irrigated-elevation ) [
    set farm 1
    set pcolor green
  ]
  if ( elevation > irrigated-elevation ) [
    set farm 1
    set pcolor lime
  ]
end

to expand-farms
  let suitableAreasConnected patches with [ farmProbability > 0 and farm = -1 and farmConnected? = true ]
  let suitableAreasDisconnected patches with [ farmProbability > 0 and farm = -1 and farmConnected? = 0 ]

  let suitableAreasConnectedCount count suitableAreasConnected
  let suitableAreasDisconnectedCount count suitableAreasDisconnected

  ; divide up additional people between connected and disconnected villages
  let additional-people-per-village additional-people / count patches with [ storageCapacity >= 0 ]
  let village-count-connected count patches with [ storageCapacity >= 0 and villageConnected? = true ]
  let village-count-disconnected count patches with [ storageCapacity >= 0 and villageConnected? = 0 ]
  let additional-people-connected additional-people-per-village * village-count-connected
  let additional-people-disconnected additional-people-per-village * village-count-disconnected

  ; calculate additional crops required as population expands
  let crop-expansion-rate-connected additional-people-connected * crops-per-person
  let crop-expansion-rate-disconnected additional-people-disconnected * crops-per-person

  ; calculate crop to farm cell conversion with yield
  let farm-cells-per-crop-connected (1 / yield-connected) * (1 / hectares-per-cell)
  let farm-cells-per-crop-disconnected (1 / yield-disconnected) * (1 / hectares-per-cell)

  ; count the number of cells to expand
  let farm-cell-expansion-rate-connected crop-expansion-rate-connected * farm-cells-per-crop-connected
  let farm-cell-expansion-rate-disconnected crop-expansion-rate-disconnected * farm-cells-per-crop-disconnected

  let min-expansion-rate-connected min list farm-cell-expansion-rate-connected suitableAreasConnectedCount
  let min-expansion-rate-disconnected min list farm-cell-expansion-rate-disconnected suitableAreasDisconnectedCount

  let farms-to-expand-connected max-n-of min-expansion-rate-connected suitableAreasConnected [ farmProbability ]
  let farms-to-expand-disconnected max-n-of min-expansion-rate-disconnected suitableAreasDisconnected [ farmProbability ]

  ask farms-to-expand-connected [
    check-elevation-and-expand-farms
  ]

  ask farms-to-expand-disconnected [
    check-elevation-and-expand-farms
  ]
end

to add-storageCapacity
  ask patches with [ storageCapacity >= 0 ] [
    ; select the farms in the radius of a village
    let localFarms patches in-radius 10 with [ farm > 0 and farmCounted? = 0 ]
    ; set them as counted to avoid double counting
    ask localFarms [
      set farmCounted? true
    ]
    let localFarmCount count localFarms
    let localProduction 0
    ifelse ( villageConnected? = true ) [
      set localProduction localFarmCount * yield-connected
    ] [
      set localProduction localFarmCount * yield-disconnected
    ]
    set capacityDifference localProduction - storageCapacity
    ifelse ( capacityDifference > 0 ) [
      output-show word "added capacity: " round capacityDifference
    ] [
      output-show "added capacity: 0"
    ]
    if ( capacityDifference > 0 ) [
      set storageCapacity storageCapacity + capacityDifference
    ]
  ]
  ; set all farms as not counted for the next time storage is added
  ask patches with [ farm > 0 ] [
    set farmCounted? 0
  ]
end

to add-villages
;  Each village is surrounded by a maximum of pi * walking distance^2 crop cells.
;  ha / village * T / ha * people / T
  let people-per-village-connected pi * walking-distance ^ 2 * yield-connected * (1 / crops-per-person)
  let people-per-village-disconnected pi * walking-distance ^ 2 * yield-disconnected * (1 / crops-per-person)

  let available-housing-connected count patches with [ storageCapacity >= 0 and villageConnected? = true ] * people-per-village-connected
  let available-housing-disconnected count patches with [ storageCapacity >= 0 and villageConnected? = 0 ] * people-per-village-disconnected

  let available-housing available-housing-connected + available-housing-disconnected

  let unhoused-people max list (population-current - available-housing) 0

  let villages-to-add ceiling (unhoused-people / people-per-village-disconnected)

  if (villages-to-add > 0) [

    ; identify perimeter patches of villages with no more area to expand into

    ask patches with [ storageCapacity >= 0 ] [
      let surroundingPatches patches in-radius walking-distance
      let perimeterPatches surroundingPatches with [ villageDistance > ( walking-distance - 1 ) ]

      let eligibleFarmPatches surroundingPatches with [ farmProbability > 0 and farm = -1 ]

      let eligibileFarmPatchCount count eligibleFarmPatches

      if ( eligibileFarmPatchCount = 0 ) [

        let eligibleVillagePatches perimeterPatches with [ roadsID = -2 and farm > 0 ]

        ask n-of 1 eligibleVillagePatches [
          set eligibleVillagePatch? true
        ]

      ]

    ]

    ; repeat the steps below each time a village is added

    if ( count patches with [ eligibleVillagePatch? = true ] > 0 ) [

      while [ villages-to-add > 0 ] [

        ; find which perimeter patches aren't close to roads

        ask patches with [ eligibleVillagePatch? = true and storageCapacity = -1 ] [

          if (any? neighbors4 with [ roadsID > -2 ]) [ set eligibleVillagePatchNearRoad? true ]

        ]

        ; find which perimeter patches aren't close to an existing village

        ask patches with [ eligibleVillagePatch? = true and storageCapacity = -1 and eligibleVillagePatchNearRoad? = 0 ] [
          let surroundingPatches patches in-radius walking-distance

          if count surroundingPatches with [ storageCapacity >= 0 ] > 1 [
            set nearOtherVillage? true

          ]

        ]

        ; out of perimeter patches not close to existing villages,
        ; calculate the amount of available farmland around them.

        ask patches with [ eligibleVillagePatch? = true and storageCapacity = -1 and eligibleVillagePatchNearRoad? = 0 and nearOtherVillage? = 0 ] [
          let surroundingPatches patches in-radius walking-distance

          ask surroundingPatches [
            set eligibleVillageDistance distance myself
          ]

          ask surroundingPatches with [ excludedClasses = -1 and eligibleVillageDistance > 0 and roadsID = -2 and storageCapacity = -1  ] [
            ; add 0.001 so probability of farthest patches is not zero
            set dummyFarmProbability 1 - ( eligibleVillageDistance / walking-distance ) + 0.001
          ]

          set dummyFarmProbabilitySum sum [ dummyFarmProbability ] of surroundingPatches with [ farm = -1 ]

        ]

        ; add a village to the perimeter patch not close to
        ; existing villages with the most available farmland

        ifelse ( count patches with [ eligibleVillagePatch? = true and storageCapacity = -1 and eligibleVillagePatchNearRoad? = 0 and nearOtherVillage? = 0 ] > 0 ) [

          ask max-one-of patches with [ eligibleVillagePatch? = true and storageCapacity = -1 and eligibleVillagePatchNearRoad? = 0 and nearOtherVillage? = 0 ] [ dummyFarmProbabilitySum ] [
            set storageCapacity 0
            set pcolor red
          ]

          set villages-to-add villages-to-add - 1

        ] [
          set villages-to-add 0
        ]

      ]

    ]

  ]

  set available-housing-connected count patches with [ storageCapacity >= 0 and villageConnected? = true ] * people-per-village-connected
  set available-housing-disconnected count patches with [ storageCapacity >= 0 and villageConnected? = 0 ] * people-per-village-disconnected

  set available-housing available-housing-connected + available-housing-disconnected

  set unhoused-people max list (population-current - available-housing) 0

  set population-current population-current - unhoused-people

end

to compute-manhattan-distances-out
  let unpavedRoads patches with [ roadsPaved = 0 ]
  let unpavedRoadsNextToPavedRoads unpavedRoads with [ any? neighbors4 with [ roadsPaved > 0 ] ]
  ask unpavedRoadsNextToPavedRoads [
    sprout 1 [
      set distanceTurtle? true
      set homeDistance 0
      set size 5
    ]
  ]
  repeat 250 [ compute-manhattan-distance-out-one-step ]
end

to compute-manhattan-distance-out-one-step
  ask turtles with [ distanceTurtle? = true ] [
    set roadStartDistance homeDistance
    let nextDistance homeDistance + 1
    let patchesToVisit neighbors4 with [ roadsID > -2 and roadsPaved = 0 and nextDistance < roadStartDistance ]
    ask patchesToVisit [
      if not any? turtles-here [
        sprout 1 [
          set distanceTurtle? true
          set homeDistance nextDistance
          set size 5
          set hidden? true
        ]
      ]
    ]
    die
  ]
end

to compute-manhattan-distances-back-setup
  ask patches with [ storageCapacity >= 0 ] [
    sprout 1 [
      set distanceTurtle? true
      set size 5
      set homeSilos 0
      set homeVillages 0
    ]
  ]

  repeat 200 [ compute-manhattan-distance-back-one-step-setup ]

end

to compute-manhattan-distance-back-one-step-setup
  ask turtles with [ distanceTurtle? = true ] [

    ; check if silo has already been counted
    let neighboringSilos neighbors4 with [ storageCapacity >= 0 and checkSilo = 0 ]
    let neighboringSilosCount count neighboringSilos
    if ( neighboringSilosCount >= 1 ) [
      ask neighboringSilos [
        set checkSilo 1
      ]
    ]

    let neighboringStorage sum [ storageCapacity ] of neighboringSilos
    let nextSilos homeSilos + neighboringStorage

    let nextVillages homeVillages + neighboringSilosCount

    ; add silo count from individual turtle to patch total
    set silosAlongRoads silosAlongRoads + nextSilos
    set villagesAlongRoads villagesAlongRoads + nextVillages

    ; necessary because once turtles reach paved roads they keep replicating
    ; and add to silo count making it become extremely high
    if not any? neighbors4 with [ roadsPaved = 1 ] [

      if any? neighbors4 with [ roadsPaved = 0 ] [
        let minPatch min-one-of neighbors4 with [ roadsPaved = 0  ] [ roadStartDistance ]
        ask minPatch [
          ;        if not any? turtles-here [
          sprout 1 [
            set distanceTurtle? true
            set size 5
            set homeSilos nextSilos
            set homeVillages nextVillages
            ;                      set hidden? true
          ]
          ;        ]
        ]
      ]
    ]
    die
  ]
end

; Create a separate go procedure
; It differs from setup because turtles are only
; counting the change in capacity and adding that to the
; total capacity stored in the patches to prioritize road
; building

to compute-manhattan-distances-back-go

  ask patches with [ storageCapacity >= 0 ] [
    sprout 1 [
      set distanceTurtle? true
      set size 5
      set homeSilos 0
      set homeVillages 0
    ]
  ]

  ; reset checks
  ; necessary to put here because in the setup procedure
  ; all silos were already checked
  ; each time the code runs through counting the silos once
  ; it needs to mark all silos as unchecked for the next run

  ask patches with [ checkSilo = 1 ] [
    set checkSilo 0
  ]

  repeat 200 [ compute-manhattan-distance-back-one-step-go ]

end

to compute-manhattan-distance-back-one-step-go

  ask turtles with [ distanceTurtle? = true ] [

    ; check if silo has already been counted
    let neighboringSilos neighbors4 with [ storageCapacity >= 0 and checkSilo = 0 and capacityDifference >= 0 ]
    let neighboringSilosCount count neighboringSilos
    if ( neighboringSilosCount >= 1 ) [
      ask neighboringSilos [
        set checkSilo 1
      ]
    ]

    let neighboringStorage sum [ capacityDifference ] of neighboringSilos
    let nextSilos homeSilos + neighboringStorage

    ; add silo count from individual turtle to patch total
    set silosAlongRoads silosAlongRoads + nextSilos

    ; necessary because once turtles reach paved roads they keep replicating
    ; and add to silo count making it become extremely high
    if not any? neighbors4 with [ roadsPaved = 1 ] [

      if any? neighbors4 with [ roadsPaved = 0 ] [
        let minPatch min-one-of neighbors4 with [ roadsPaved = 0  ] [ roadStartDistance ]
        ask minPatch [
          ;        if not any? turtles-here [
          sprout 1 [
            set distanceTurtle? true
            set size 5
            set homeSilos nextSilos
            set hidden? true
          ]
          ;        ]
        ]
      ]
    ]
    die
  ]
end

to normalize-criteria-values
  if ( count patches with [ roadsPaved = 0 ] > 0 ) [
    ; find max values of criteria
    let unpavedRoads patches with [ roadsPaved = 0 ]
    let maxSilosAlongRoads max [ silosAlongRoads ] of unpavedRoads
    let maxVillagesAlongRoads max [ villagesAlongRoads ] of unpavedRoads

    ask unpavedRoads [
      ifelse ( maxSilosAlongRoads > 0 ) [
        set normalizedSilosAlongRoads precision (silosAlongRoads / maxSilosAlongRoads) 3
      ] [
        set normalizedSilosAlongRoads 0
      ]
;      if ( silosAlongRoads > 0 ) [
;        set normalizedSilosAlongRoads ( 1 - normalizedSilosAlongRoads )
;      ]
      ifelse ( maxVillagesAlongRoads > 0 ) [
        set normalizedVillagesAlongRoads precision (villagesAlongRoads / maxVillagesAlongRoads) 3
      ] [
        set normalizedVillagesAlongRoads 0
      ]
    ]

    ask roads [
      set my-avoided-flood-sum sum [ avoidedFloodProbability ] of my-patches
      ifelse ( my-length > 0 ) [
        set my-avoided-flood-proportion my-avoided-flood-sum / my-length
        set my-max-storage max [ normalizedSilosAlongRoads ] of my-patches
        set my-max-villages max [ normalizedVillagesAlongRoads ] of my-patches
      ] [
        set my-avoided-flood-proportion 0
        set my-max-storage 0
        set my-max-villages 0
      ]
      set my-criteria-sum  flood-weight * my-avoided-flood-proportion + storage-weight * my-max-storage + village-weight * my-max-villages
    ]

  ]
end

to pave-roads

;  ask roads [
;    if my-length > 0 [
;      set my-min-start-distance min [ roadStartDistance ] of my-patches
;    ]
;  ]

  ifelse ( count roads with [ my-paved = 0 and ( my-max-villages > 0 or my-max-storage > 0) ] > 0 ) [

    set current-roads-budget ( roads-budget + remaining-roads-budget )

    while [ current-roads-budget > 0 and count roads with [ my-paved = 0 and ( my-max-villages > 0 or my-max-storage > 0) ] > 0 ] [

      let roads-to-pave roads with [ my-paved = 0 and ( my-max-villages > 0 or my-max-storage > 0) ]

      let road-to-pave max-one-of roads-to-pave [ my-criteria-sum ]

      if [ my-length ] of road-to-pave < current-roads-budget [

        ask road-to-pave [
          set my-paved 1
          ask my-patches [
            set pcolor gray
            set roadsPaved 1
          ]
          ask my-patches with [ storageCapacity >= 0 ] [
            set pcolor red
          ]
          output-show word "Pave road " my-roads-ID
        ]



      ]

      set remaining-roads-budget current-roads-budget

      set current-roads-budget current-roads-budget - [ my-length ] of road-to-pave

    ]

  ] [

    let msg (word "No more suitable roads to pave at tick " ticks)
    show msg
    set simulation_complete true

  ]
end

to calculate-network-speed
  let totalRoads count patches with [ roadsID > -2 ]
  let pavedRoads count patches with [ roadsID > -2 and roadsPaved = 1 ]
  let currentPavedRatio pavedRoads / totalRoads
  set networkSpeed ( 17.21 / initialPavedRatio ) * currentPavedRatio
end

to grow-population
  set population-next population-current * ( 1 + population-growth )
  set additional-people population-next - population-current
  set population-current population-next
end

to calculate-crop-quantity
;  farm cells * ha / cell * T / ha
  let crop-quantity-connected count patches with [ farm > 0 and farmConnected? = true ] * hectares-per-cell * yield-connected
  let crop-quantity-disconnected count patches with [ farm > 0 and farmConnected? = 0 ] * hectares-per-cell * yield-disconnected
  set crop-quantity crop-quantity-connected + crop-quantity-disconnected
end

to calculate-crops-per-person
  let initial-crop-quantity crop-quantity
  set crops-per-person initial-crop-quantity / initial-population
;  set crops-per-person 5
end

to export-world-raster
  set world_raster gis:patch-dataset pcolor
  let directory "C:/Users/Sensonomic Admin/Desktop/world_raster_export_files/world_raster_export"
  gis:store-dataset world_raster word directory ticks
end

to show-year
  output-show (word "Year " (ticks + 2018))
end

to count-villages-along-paved

  set villages-along-paved count patches with [ storageCapacity >= 0 and villageConnected? = true ]

end

to calculate-total-storage-added-each-tick

  set total-storage-added sum [ storageCapacity - initialStorage ] of patches with [ storageCapacity >= 0 ]

end

to calculate-total-storage-added

  ask patches with [ storageCapacity >= 0 ] [
    let capacity-difference round storageCapacity - initialStorage
    output-show word "total added capacity: " capacity-difference
  ]

end

to check-villages-connected
  ask patches with [ storageCapacity >= 0 ] [
    if (any? neighbors4 with [ roadsPaved = 1 ]) [ set villageConnected? true ]
  ]
end

to check-farms-connected
  ask patches with [ storageCapacity >= 0 and villageConnected? = true ] [
    ask patches in-radius walking-distance [
      set farmConnected? true
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
303
10
1100
808
-1
-1
3.274
1
10
1
1
1
0
0
0
1
-120
120
-120
120
0
0
1
ticks
30.0

BUTTON
87
92
150
125
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
168
93
231
126
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
39
674
257
707
flood-risk-elevation
flood-risk-elevation
0
20
5.0
1
1
m
HORIZONTAL

SLIDER
43
500
263
533
travel-distance
travel-distance
1
5
4.0
1
1
km
HORIZONTAL

OUTPUT
1119
11
1426
807
11

SLIDER
41
626
257
659
roads-investment
roads-investment
0
500
150.0
25
1
million CFA
HORIZONTAL

SLIDER
48
269
262
302
population-growth-rate
population-growth-rate
0
20
10.4
0.1
1
%
HORIZONTAL

PLOT
1448
10
1648
160
Population
years
people
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot population-current"

PLOT
1449
181
1649
331
Crop quantity
years
tons
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot crop-quantity"

SWITCH
307
829
431
862
add-storage
add-storage
0
1
-1000

SLIDER
38
726
256
759
flood-weight
flood-weight
0
10
1.0
1
1
NIL
HORIZONTAL

SLIDER
39
780
252
813
storage-weight
storage-weight
0
10
1.0
1
1
NIL
HORIZONTAL

SLIDER
39
832
253
865
village-weight
village-weight
0
10
1.0
1
1
NIL
HORIZONTAL

TEXTBOX
52
201
202
219
Crops
11
0.0
1

TEXTBOX
45
595
195
613
Roads
11
0.0
1

CHOOSER
46
147
194
192
community
community
"bandafassi" "ndorna" "makacoulibantang"
0

SLIDER
46
314
267
347
hectares-per-household
hectares-per-household
0
3
1.5
0.5
1
ha
HORIZONTAL

SLIDER
41
549
266
582
irrigated-elevation
irrigated-elevation
0
10
3.0
1
1
m
HORIZONTAL

SLIDER
47
225
262
258
initial-population
initial-population
1000
50000
12000.0
1000
1
people
HORIZONTAL

SLIDER
46
361
264
394
people-per-household
people-per-household
0
15
10.0
1
1
people
HORIZONTAL

PLOT
1448
355
1648
505
Villages reached
years
villages
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot villages-along-paved"

PLOT
1452
522
1652
672
Storage added
years
tons
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-storage-added"

BUTTON
1667
576
1863
609
NIL
calculate-total-storage-added
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
47
10
286
70
file-path
C:/Users/Sensonomic Admin/Dropbox/Oxford/DPhil/Sensonomic/RiceScape_GitHub/Ricescape
1
0
String (commands)

SLIDER
1464
725
1636
758
hectares-per-cell
hectares-per-cell
1
10
5.29
0.01
1
NIL
HORIZONTAL

SLIDER
46
407
263
440
yield-connected
yield-connected
0
10
6.0
1
1
NIL
HORIZONTAL

SLIDER
45
453
263
486
yield-disconnected
yield-disconnected
0
10
3.0
1
1
NIL
HORIZONTAL

SWITCH
480
827
629
860
add-new-villages
add-new-villages
0
1
-1000

PLOT
1675
182
1875
332
Crop area
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count patches with [ farm > 0 ] - initial-farm-count"

MONITOR
1701
369
1770
414
Crop area
count patches with [ farm > 0 ] - initial-farm-count
17
1
11

SLIDER
1470
781
1642
814
cells-per-km
cells-per-km
1
10
4.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

Inside the limits of a given budget, how should road infrastructure upgrades be done around the town of Bandafassi region of Senegal.

Optimizing for rice production, RiceScape is the NetLogo core file, providing a complete NetLogo code, documentation and interface for modeling geospatial problems through individual agents in agricultural Senegal . The RiceScape project by Sensonomic Ltd. enrich data from the consortium with members of European space and Space scene, and the product relies on value streams from our partners in Vito, Geoville and the Wageningen University (WUR), and our data providers ESA and NASA.

As this model supports the operations of the United Nations through the Internatiional Fund for Agriculture and Development, it models Bandafassi as an agricultural system. 


## HOW IT WORKS

The model consists of farm patches, road patches and storage agents.  The key question this model seeks to answer is: Which roads should be upgraded, optimizing for production?

Rice is produced by farm patches. Farm patches have a spawn rate, after an initial seeding based off of rice paddy data collection provided to Sensonomic by consortium partners. Farm patches have properties of yield

To produce 


(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES
|
## RELATED MODELS

This model relies on the 

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
