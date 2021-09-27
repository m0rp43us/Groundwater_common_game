; THE GROUNDWATER COMMONS GAME — Developed using FlowLogo, copyright 2017 Juan Carlos Castilla-Rho

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; LOAD EXTENSIONS ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

extensions [matrix]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GLOBAL VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

globals
[
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;; CONTROL ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;time;;;;;;
  year
  month
  growing-season?
  managent-period-started?

  ;;;;;I/O;;;;;;;
  SGS-filename

  ;;;;;lorenz and gini;;;;;
  gini-index-reserve
  lorenz-points
  sorted-wealths

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;; ABM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  compliance-level                             ;; the compliance % = #cooperators / (#cooperators+#defectors)
  number-of-farmers-with-voluntary-compliance  ;; keeps a count of farmers choosing voluntary compliance
  number-of-farmers-with-rule-breaker          ;; keeps a count of farmers choosing rule breaking
  defectors                                    ;; the agentset of defecting farmers
  cooperators                                  ;; the agentset of cooperating farmers
  cell-area                                    ;; the area of one cell in hectares
  model-ready?                                 ;; flag used to initialise ABM plots
  agents-sorted-by-overall-severity            ;; list of agents used to apply graduated sanctions
  farmers-inspected-this-turn                  ;; agentset used to apply graduated sanctions
  tier-1                                       ;; agentset used to apply graduated sanctions
  tier-2                                       ;; agentset used to apply graduated sanctions
  tier-3                                       ;; agentset used to apply graduated sanctions
  decision-of-representative-farmer            ;; stores the decision of a representative farmer (used to create TS list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; CROP ECONOMICS ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  income-per-hectare                           ;; $/ha income for the crop
  fixed-costs-per-hectare                      ;; $/ha of fixed costs
  irrigation-variable-costs-per-hectare        ;; $/ha of irrigation costs                       ************* (FUNCTION OF CURRENT GW DRAWDOWN) ***********
  other-variable-costs-per-hectare             ;; $/ha of other variable costs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; INSTITUTION ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  monitoring-percent                       ;; the probability of a fine at each moment, function of compliance-level

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; GW ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;groundwater model formulation;;;;;;
  max-error
  max-head
  min-head
  max-K
  min-K
  iterations
  aquifer-width
  A
  A-row-list
  A-row-reduced-list
  A-row-reduced-matrix
  A-column-list
  A-column-reduced-list
  A-reduced
  inverse-A
  C
  C-row-list
  C-row-reduced-list
  C-row-reduced-matrix
  C-reduced
  solution-vector
  number-of-unknowns
  K-patch-read

  ;;;;;;;;lists of patches;;;;;;;
  no-flow-cells-list
  fixed-head-cells-list
  remove-cells-list
  remove-cells-list-corrected
  active-cells-remap
  active-cells-remap-indexes

  ;;;;;;;multiplier functions;;;;;;;
  sine-recharge
  sine-well

  ;;;;;;river budget term;;;;;;;;
  riv-budget

  ;;;;;;well pumping budget term;;;;;;;;
  well-budget

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;; EXPERIMENTS I/O ;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  filename

  max-monitoring-capacity_name
  fine-magnitude_name
  S-enforcement-cost_name
  S-reputation_name
  voluntary-compliance-level_name
  rule-breaker-level_name
  metanorm_name

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;; EXPERIMENTS TIMESERIES ;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  TS-norm-strength
  TS-compliance
  TS-gini-wealth
  TS-decision-representative-farmer
  TS-drawdowns-mean
  TS-drawdowns-std
  TS-profits-mean
  TS-cummulative-wealth-median
  TS-total-breaches
  TS-boldness
  TS-vengefulness
]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AGENT BREEDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

breed [farmers farmer]
breed [limits limit]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AGENT VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

farmers-own
[
  farm-area-total                      ;;[ha]
  entitlement                          ;;[ML]
  allocation                           ;;[ML]

  GM                ;; gross margin per hectare
  IN                ;; income per hectare
  VC                ;; variable costs per hectare, total
  VC-other          ;; variable costs per hectare, other than pumping/energy expenses
  VC-pumping        ;; variable costs per hectare, pumping/energy expenses

  Q-farm            ;; pumping rate for the growing season [m3/d]
  breach            ;; the breach of a defecting farmer
  irrigated-area    ;; the area that is irrigated given a decision to defect/cooperate [ha]

  boldness          ;;the farmer's boldness [0,1]
  vengefulness      ;;the farmer's vengefulness [0,1]
  decision          ;;the farmer's decision ["cooperate","defect"]

  Hlift
  Hground

  times-punished-others
  times-caught-defecting
  times-reported
  neighbours-that-punished

  bad-monitor?

  caught-this-year?
  caught-last-year?
  breach-severity
  repetition-severity
  overall-severity-number
  overall-severity-category
  graduated-fine

  farmer-compliance-risk

  score
  score-history-list
  score-accumulated

  voluntary-compliance?
  rule-breaker?

  E-score-winter
  E-score-summer
  E-score
  I-score
  S-score

  OD
  s-total
  Srand
  group
  distance-list
  damage-list
  long-neighbors-list
  neighbors-list
  seen-defecting?
  being-bad-monitor?
  seen-being-bad-monitor?
  this-turns-reputation
  reputation-history-list
  reputation-accumulated
  strong-belief?
  ranking-drawdown

  cummulative-wealth
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PATCH VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

patches-own
[
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;; SOCIAL ;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  is-patch-in-border?
  owner
  my-neighbors

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;; GW ;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;source and sink terms;;;;;;;

  Qwell                                    ;; well pumping rate                                        [L3/T]
  Qwell-temp                               ;; temporary dummy variable to initialize heads

  Qinjection                               ;; well injection rate                                      [L3/T]
  Qinjection-temp                          ;; temporary dummy variable to initialize heads

  Recharge                                 ;; recharge                                                 [L3/L2*T] ~ [L/T]
  Recharge-temp                            ;; temporary dummy variable to initialize heads

  ET                                       ;; evapotranspiration, function of depth to water table     [L3/L2*T] ~ [L/T]
  ET-max-rate-temp                         ;; temporary dummy variable to initialize heads
  ET-max-rate-patch
  ET-extinction-depth-patch
  ET-land-surface-elevation-patch

  DRAIN                                    ;; drain discharge, depends on h-d             [L3/T] Note: divide by cell area in sourceterm equation
  DRAIN-elevation-patch
  DRAIN-conductance-patch

  RIV                                      ;; leakage from river                          [L3/L2*T] ~ [L/T]
  RIV-elevation-patch
  RIV-bottom-patch
  RIV-conductance-patch

  ;;;;;;patch hydraulic parameters;;;;;;;
  S-patch                                  ;; storativity of each patch
  S-patch-temp                             ;; temporary dummy variable to solve for steady state
  K-patch                                  ;; conductivity of each patch
  T-patch                                  ;; transmissivity of each patch

                                           ;;;;;;;;;type of patch;;;;;;;;
  interior-node?                           ;; is this an active patch?
  adjacent-to-boundary?                    ;; is this patch adjacent to a boundary?
  interior-but-corner?                     ;; out of the interior nodes, is it in the corner?

                                           ;;;;;;patch bc's;;;;;
  no-flow?                                 ;; is this a no-flow cell?
  fixed-head?                              ;; is this a fixed head cell?
  fixed-flux?                              ;; is this a fixed flux cell?

                                           ;;;;;;patch neighbours;;;;
  N-neighbour
  S-neighbour
  E-neighbour
  W-neighbour

  ;;;;;;patch features;;;;;
  well?
  injection?
  recharge?
  ET?
  DRAIN?
  RIV?

  ;;;;;;;head values;;;;;;
  H                                       ;; heads at the new timestep
  Hinew                                   ;; heads at the new iteration
  Hinitial                                ;; a reference H to calculate drawdowns

                                          ;;;;;;solution process parameters;;;;;
  TN
  TS
  TE
  TW
  TC
  Ncof
  Scof
  Ecof
  Wcof
  Ccof
  SourceTerm
  RHS
  patch-number-tag
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN - SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;to setup
;  clear-all
;  setup-GWmodel
;  if social-model? = true [setup-SOCIALmodel]
;end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; SOCIAL SETUP ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to SETUP-RANDOM
  ask turtles [die]
  SETUP-FARMERS-RANDOM
  CALCULATE-FARM-AREAS
  UPDATE-COMPLIANCE-LEVEL
  DEFINE-NEIGHBOURS
  reset-ticks
  set growing-season? "winter"
  set model-ready? true
  UPDATE-LORENZ-AND-GINI
  INITIALISE-TS-LISTS
end

;;FARMERS WITH A RANDOM DISTRIBUTION OF STRATEGIES
to SETUP-FARMERS-RANDOM
  TAG-PATCHES-IN-BORDER
  ask n-of num-farmers patches with [is-patch-in-border? = false and injection? = false]
  [
    sprout-farmers 1
    [
      set shape "circle"
      set size 0.5
      set boldness (random 11 / 10)                          ;; gives each farmers an initial random boldness
      set vengefulness (random 11 / 10)                      ;; gives each farmers an initial random vengefulness
      DECIDE-DEFECT-OR-NOT
      CALCULATE-FARMER-COLOURS
      ;set reputation-accumulated 0
      ;set reputation-history-list []
      ;set score-history-list []
      set Qwell 0
      set well? true
      set voluntary-compliance? false
      set rule-breaker? false
      set score 1
      set E-score 1
      set cummulative-wealth 1
      set score-history-list []
      set score-history-list lput 10 score-history-list
    ]
  ]
  ask patches with [is-patch-in-border? = false and injection? = false]           ;; create land plots - each cell goes to the closest farmer
  [
    let my-farmer min-one-of farmers [distance myself]
    set pcolor 57                                          ;; a green background — better contrast with colour coding of farmers
    set owner my-farmer
    sprout-limits 1                                        ;; creates 1 limit turtle on each patch and hide it for now
  ]
  create-fronteers
end


;; RETURNS A LIST OF NEIGHBORS FOR EACH FARMER
to DEFINE-NEIGHBOURS
  ask patches with [is-patch-in-border? = false and injection? = false] [set my-neighbors [owner] of [neighbors with [is-patch-in-border? = false and injection? = false]] of self]
  ask farmers
  [
    set long-neighbors-list [my-neighbors] of patches with [is-patch-in-border? = false and injection? = false and owner = myself]
    set neighbors-list other turtle-set long-neighbors-list
  ]
end

;; CREATES THE LIMITS FOR EACH FARM
to create-fronteers
  ask patches with [is-patch-in-border? = false and injection? = false] [
    ifelse patch-at 0 -1 != nobody and owner != [owner] of patch-at 0 -1 [             ;; if patch just below is different then
      ifelse patch-at 1 0 != nobody and owner != [owner] of patch-at 1 0 [             ;; if patch just on the right is different then there should be also a low and right fronteer
        ask one-of limits-here [
          set shape "line right and below"
          set color brown
          st
        ]
      ]
      [
        ask one-of limits-here [                                                    ;; if patch just on the right is not different then there should be a low fronteer
          set shape "line bottom"
          set color brown
          st
        ]
      ]
    ]
    [                                                                               ;; if patch just below is not different then
      ifelse patch-at 1 0 != nobody and owner != [owner] of patch-at 1 0 [          ;; if patch just on the right is different then there should be also a  right fronteer
        ask one-of limits-here [
          set shape "line right"
          set color brown
          st]
      ][
      ; if patch just on the right is not different then there should be nothing
      ask one-of limits-here [ht]
      ]
    ]
  ]
end

to TAG-PATCHES-IN-BORDER
  ask patches [set is-patch-in-border? false]
  ask patches with [pxcor <= min-pycor + buffer][set is-patch-in-border? true]
  ask patches with [pxcor >= max-pxcor - buffer][set is-patch-in-border? true]
  ask patches with [pycor >= max-pycor - buffer][set is-patch-in-border? true]
  ask patches with [pycor <= min-pycor + buffer][set is-patch-in-border? true]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; GW SETUP ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup-GWmodel
  ca
  reset-ticks
  setup-world
  setup-bc
  setup-initial-heads
  setup-hydraulic-parameters
  setup-sources
  setup-maxmin-heads
  setup-neighbours
  setup-patch-numbering
  setup-patches-adjacent-to-boundary
  refresh-view
end

to setup-world
  resize-world 0 (N + 1) 0 (M + 1)                            ;; defines number of cells in X (N) and Y (M) direction, leaves additional patches for the boundary conditions NOTE: N=M
  ask patches [set pcolor white]                              ;; set initial colour of patches to white
  ask patches [set well? false set recharge? false]           ;; initially none of the patches have wells or recharge, setup-pumping and setup-recharge modify this tag
end

to setup-bc
  ask patches [set no-flow? false set fixed-head? false set interior-node? true]                                                            ;; initialize these location indicators: all nodes are interior and no bc tags - easier for further coding

  ask patches [if (pxcor = min-pxcor) or (pxcor = max-pxcor) or (pycor = max-pycor) or (pycor = min-pycor) [set interior-node? false]]      ;; tag interior and boundary nodes

  ifelse left-bc = "no-flow"
  [ask patches with [pxcor = 0][set pcolor black set no-flow? true set interior-node? false set fixed-head? false set K-patch 0 set T-patch 0]]
  [ask patches with [pxcor = 0][set pcolor blue set fixed-head? true set interior-node? false set H left-bc-head]]

  ifelse right-bc = "no-flow"
  [ask patches with [pxcor = max-pxcor][set pcolor black set no-flow? true set interior-node? false set fixed-head? false set K-patch 0 set T-patch 0]]
  [ask patches with [pxcor = max-pxcor][set pcolor blue set fixed-head? true set interior-node? false set H right-bc-head]]

  ifelse top-bc = "no-flow"
  [ask patches with [pycor = max-pycor][set pcolor black set no-flow? true set interior-node? false set fixed-head? false set K-patch 0 set T-patch 0]]
  [ask patches with [pycor = max-pycor][set pcolor blue set fixed-head? true set interior-node? false set H top-bc-head]]

  ifelse bottom-bc = "no-flow"
  [ask patches with [pycor = min-pycor][set pcolor black set no-flow? true set interior-node? false set fixed-head? false set K-patch 0 set T-patch 0]]
  [ask patches with [pycor = min-pycor][set pcolor blue set fixed-head? true set interior-node? false set H bottom-bc-head]]
end

to setup-initial-heads
  ask patches with [interior-node? = true] [set H initial-heads]                               ;; set the initial heads in interior nodes
end

to set-initial-heads
  ask patches with [interior-node? = true][set Hinitial H]
end

to setup-hydraulic-parameters                                       ;; sets a homogeneous conductivity to the whole model using the input box on the interface (avoids having cells with no K value)
  ask patches with [no-flow? = false]
  [
    set K-patch K
    ifelse aquifer-type = "confined"
    [set T-patch (K-patch * aquifer-thickness) set S-patch S]       ;; sets transmissivity and storage for confined conditions
    [set T-patch (K-patch * H) set S-patch Sy]                      ;; sets transmissivity and storage for unconfined conditions
  ]
end

to setup-sources
  ask patches [set well? false]         ;; initialize patch tags without wells
  ask patches [set injection? false]    ;; initialize patch tags without injection
  ask patches [set recharge? false]     ;; initialize patch tags without recharge
  ask patches [set ET? false]           ;; initialize patch tags without ET
  ask patches [set DRAIN? false]        ;; initialize patch tags without DRAIN
  ask patches [set RIV? false]          ;; initialize patch tags without leakage from river

                                        ;; Sources and stresses can be added directly through the interface, otherwise manually (directly in the code) here:

                                        ;; pumping wells, units of [m3/day] ~ [L3/T]
                                        ;; injection wells, units of [m3/day] ~ [L3/T]
                                        ;; recharge cells, units of [L3/L2*T] ~ [L/T]
                                        ;; ET cells, units of [L3/L2*T] ~ [L/T]
                                        ;; DRAIN cells, units of [L3/T]
                                        ;; RIV cells, units of [L3/T]
end

to setup-neighbours
  ask patches with [interior-node? = true]
  [
    set N-neighbour patch-at 0 1                                          ;; neighbour North of this patch
    set S-neighbour patch-at 0 -1                                         ;; neighbour South of this patch
    set E-neighbour patch-at 1 0                                          ;; neighbour East of this patch
    set W-neighbour patch-at -1 0                                         ;; neighbour West of this patch
  ]
end


to setup-patch-numbering
  ask patches with [interior-node? = true]
  [set patch-number-tag pxcor + (M - pycor) * N]
end


to setup-patches-adjacent-to-boundary
  ask patches with [interior-node? = true]
  [
    ifelse (pxcor = (min-pxcor + 1) or pxcor = (max-pxcor - 1) or pycor = (min-pycor + 1) or pycor = (max-pycor - 1))
    [set adjacent-to-boundary? true]
    [set adjacent-to-boundary? false]
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; INITIAL HEADS — STEADY STATE RUN WITH NO STRESSES ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to initialize-heads                                                    ;; runs the model ONCE, with all stresses = 0 to set up the initial heads
  reset-ticks

  ask patches
    [
      if aquifer-type = "confined"                                     ;; sets transmissivity and storage for confined conditions
      [set T-patch (K-patch * aquifer-thickness) set S-patch S]

      if aquifer-type = "unconfined"                                   ;; sets transmissivity and storage for unconfined conditions
      [set T-patch (K-patch * H) set S-patch Sy]

      set Qwell-temp Qwell                                             ;; save the original values to restore after the init, they are needed for the steady/transient simulation
      set Qinjection-temp Qinjection
      set Recharge-temp Recharge
      set S-patch-temp S-patch
      set ET-max-rate-temp ET-max-rate-patch

      set Qwell 0                                                      ;; we set these parameters to zero for the init, which is steady state without any stresses
      set Qinjection 0
      set Recharge 0
      set S-patch 0
      set ET-max-rate-patch 0
    ]

  ask patches with [ET? = true]                                      ;; sets the ET cell to a fixed-head condition to give a reasonalble steady-state solution considering long-term equilibrium at H = ground elevation - extinction depth
    [
      set H (ET-land-surface-elevation - ET-extinction-depth-patch)
      set fixed-head? true
    ]

  prepare-equations                                                  ;; set up the equations for this solution
  go-GW

  refresh-view

  ;; now prepare equations for the main simulation

  if solver = "steady-state" [prepare-equations-steadystate]                                                   ;; note that the fixed-head assumption for ET cells is maintained here
  if solver = "transient" [ask patches with [ET? = true][set fixed-head? false] prepare-equations-transient]   ;; remove the fixed-head assumption of ET cells for subsequent transient simulations
end

to prepare-equations-steadystate
  ask patches
  [
    set Qwell Qwell-temp
    set Qinjection Qinjection-temp
    set Recharge Recharge-temp
    set S-patch 0                                          ;; S=0 for steady state simulations and stresses are activated
    set ET-max-rate-patch ET-max-rate-temp
  ]
  prepare-equations
end

to prepare-equations-transient
  ask patches
  [
    set Qwell Qwell-temp
    set Qinjection Qinjection-temp
    set Recharge Recharge-temp
    set S-patch S-patch-temp
    set ET-max-rate-patch ET-max-rate-temp
  ]                                                        ;; restores original parameters for the simulation
  prepare-equations
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; PREPARE EQUATIONS AND DEFINE THE INVERSE OF MATRIX A ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; executed ONCE for CONFINED conditions (T does not change during the simulation) and ITERATIVELY for unconfined conditions (T=Kxh varies during the simulation)

to prepare-equations
  calculate-conductances
  modify-conductances-for-bcs
  build-matrix-A
  remove-inactive-cells-from-A-matrix
  calculate-inverse-A
end

to calculate-conductances                                                ;; sets the interblock transmissivities using the harmonic mean
  ask patches with [interior-node? = true]
  [
    set TN [T-patch] of N-neighbour                                        ;; T of patch North of here
    set TS [T-patch] of S-neighbour                                        ;; T of patch South of here
    set TE [T-patch] of E-neighbour                                        ;; T of patch East of here
    set TW [T-patch] of W-neighbour                                        ;; T of patch West of here
    set TC [T-patch] of self

    ;; using the harmonic mean of conductivity

    set Ncof (1 / (delta ^ 2)) * (2 * TC * TN) / (TC + TN)                 ;; Coeff for head North of this patch
    set Scof (1 / (delta ^ 2)) * (2 * TC * TS) / (TC + TS)                 ;; Coeff for head South of this patch
    set Ecof (1 / (delta ^ 2)) * (2 * TC * TE) / (TC + TE)                 ;; Coeff for head East of this patch
    set Wcof (1 / (delta ^ 2)) * (2 * TC * TW) / (TC + TW)                 ;; Coeff for head West of this patch
    set Ccof -1 * (Ncof + Scof + Ecof + Wcof + S-patch / delta-t)          ;; Coeff for head at this patch
  ]
end

to modify-conductances-for-bcs
  ask patches with [interior-node? = true]
  [
    ;; conductance to/from no-flow boundaries are zero

    if [no-flow?] of N-neighbour = true [set Ncof 0]
    if [no-flow?] of S-neighbour = true [set Scof 0]
    if [no-flow?] of E-neighbour = true [set Ecof 0]
    if [no-flow?] of W-neighbour = true [set Wcof 0]

    set Ccof -1 * (Ncof + Scof + Ecof + Wcof + (S-patch / delta-t))                ;; needed to reflect any of the above changes
  ]
end

to build-matrix-A                                                                ;; AxB=C ~ NOTE: N=M
  set number-of-unknowns N * M

  ;set C matrix:make-constant (number-of-unknowns) 1 0                           ;; a column vector with 0's: here goes the SourceTerm + terms derived from bc's
  set A matrix:make-constant number-of-unknowns number-of-unknowns 0             ;; a N^2xM^2 matrix of 0's: here we will add the equation coeffs

  ask patches with [interior-node? = true][set interior-but-corner? false]       ;; set all the interior nodes to false, we tag the corner ones later

                                                                                 ;; COMPLETE THE "A" MATRIX

                                                                                 ;; interior patches not adjacent to a boundary have all the the terms
  ask patches with [interior-node? = true and adjacent-to-boundary? = false]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position - N) [Ncof] of self    ;; set the north term
    matrix:set A (my-position) (my-position - 1) [Wcof] of self    ;; set the west term
    matrix:set A (my-position) (my-position + 1) [Ecof] of self    ;; set the east term
    matrix:set A (my-position) (my-position + N) [Scof] of self    ;; set the south term
  ]

  ;; top left corner
  ask patches with [interior-node? = true and pycor = max-pycor - 1 and pxcor = min-pxcor + 1]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position + 1) [Ecof] of self    ;; set the east term
    matrix:set A (my-position) (my-position + N) [Scof] of self    ;; set the south term

    set interior-but-corner? true
  ]

  ;; top right corner
  ask patches with [interior-node? = true and pycor = max-pycor - 1 and pxcor = max-pxcor - 1]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position - 1) [Wcof] of self    ;; set the west term
    matrix:set A (my-position) (my-position + N) [Scof] of self    ;; set the south term

    set interior-but-corner? true
  ]

  ;; bottom left corner
  ask patches with [interior-node? = true and pycor = min-pycor + 1 and pxcor = min-pxcor + 1]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position - N) [Ncof] of self    ;; set the north term
    matrix:set A (my-position) (my-position + 1) [Ecof] of self    ;; set the east term

    set interior-but-corner? true
  ]

  ;; bottom right corner
  ask patches with [interior-node? = true and pycor = min-pycor + 1 and pxcor = max-pxcor - 1]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position - N) [Ncof] of self    ;; set the north term
    matrix:set A (my-position) (my-position - 1) [Wcof] of self    ;; set the west term

    set interior-but-corner? true
  ]

  ;; top row
  ask patches with [interior-node? = true and pycor = max-pycor - 1 and interior-but-corner? = false]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position - 1) [Wcof] of self    ;; set the west term
    matrix:set A (my-position) (my-position + 1) [Ecof] of self    ;; set the east term
    matrix:set A (my-position) (my-position + N) [Scof] of self    ;; set the south term
  ]

  ;; bottom row
  ask patches with [interior-node? = true and pycor = min-pycor + 1 and interior-but-corner? = false]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position - N) [Ncof] of self    ;; set the north term
    matrix:set A (my-position) (my-position - 1) [Wcof] of self    ;; set the west term
    matrix:set A (my-position) (my-position + 1) [Ecof] of self    ;; set the east term
  ]

  ;; left column
  ask patches with [interior-node? = true and pxcor = min-pxcor + 1 and interior-but-corner? = false]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position - N) [Ncof] of self    ;; set the north term
    matrix:set A (my-position) (my-position + 1) [Ecof] of self    ;; set the east term
    matrix:set A (my-position) (my-position + N) [Scof] of self    ;; set the south term
  ]

  ;; right column
  ask patches with [interior-node? = true and pxcor = max-pxcor - 1 and interior-but-corner? = false]
  [
    let my-position ([patch-number-tag] of self - 1)               ;; NOTE: indexing of the matrix extension starts at 0, not 1

    matrix:set A (my-position) (my-position) [Ccof] of self        ;; set the center term (diagonal)
    matrix:set A (my-position) (my-position - N) [Ncof] of self    ;; set the north term
    matrix:set A (my-position) (my-position - 1) [Wcof] of self    ;; set the west term
    matrix:set A (my-position) (my-position + N) [Scof] of self    ;; set the south term
  ]
end

to calculate-inverse-A
  set inverse-A matrix:inverse A
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN LOOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  if social-model? = true
  [
    if growing-season? = "winter"
    [
      SET-WINTER-CROP-ECONOMICS
      UPDATE-PUMPING-RATES
    ]
  ]

  go-GW
  refresh-view

  if social-model? = true
  [
    CALCULATE-PUMPING-LIFT
    CALCULATE-E-SCORE-WINTER
  ]

  set growing-season? "summer"
  tick
  refresh-view

  if social-model? = true
  [
    if growing-season? = "summer"
    [
      SET-SUMMER-CROP-ECONOMICS
      UPDATE-PUMPING-RATES
    ]
  ]

  go-GW
  refresh-view

  if social-model? = true
  [
    CALCULATE-PUMPING-LIFT
    CALCULATE-E-SCORE-SUMMER
  ]

  if social-model? = true
  [
    UPDATE-DEFECTOR-AND-COOPERATOR-LISTS                      ;; who cooperated and who defected in this season
    UPDATE-FARMER-COMPLIANCE-RISK                             ;; update the risk of non-compliance for each farmer
    CLEAR-SOCIAL-COUNTERS                                     ;; reset counters from previous iteration
    DECIDE-PUNISH-OR-NOT                                      ;; decide if I will punish others

    UPDATE-SCORES                                             ;; calculate everyone's EIS score for this season

    EVOLVE-STRATEGY                                           ;; select a strategy for the following season, based on this season's scores
    DECIDE-DEFECT-OR-NOT                                      ;; decide if I will defect or cooperate next season
    UPDATE-COMPLIANCE-LEVEL                                   ;; recalculate compliance %
    CALCULATE-FARMER-COLOURS                                  ;; refresh agent colour coding

    UPDATE-LORENZ-AND-GINI
    COUNT-FARMERS-WITH-VOLUNTARY-COMPLIANCE
    COUNT-FARMERS-WITH-RULE-BREAKER

    UPDATE-CUMMULATIVE-INDICATORS
    SAVE-METRICS-TO-TS-LISTS


  ]

  set growing-season? "winter"

  tick
  set year year + 1

  if managent-period-started? = True and ticks > 1 [
    ask farmers [set pcolor 57]
    ask farmers-inspected-this-turn [set pcolor blue]
  ]

  refresh-view
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; GW LOOP ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go-GW
  if aquifer-type = "unconfined" [prepare-equations solve-once update-unconfined-transmissivities]
  if aquifer-type = "confined" [solve-once]
end

to update-unconfined-transmissivities
  ask patches [set T-patch (K-patch * H)]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SOCIAL - PROCESSES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to CALCULATE-FARM-AREAS
  set cell-area (delta ^ 2) / 10000                                                           ;; area of a single cell in hectares
  ask farmers [set farm-area-total count patches with [owner = myself] * cell-area]           ;; calculate area of each farm in hectares
end

to UPDATE-PUMPING-RATES                                                                       ;; take the allocation, or allocation < Q < maxQ
  ask farmers
  [
    if decision = "cooperate"
    [
      set breach 0
      set irrigated-area (farm-area-total * pumping-cap)                                      ;; the area that the farmer has chosen to irrigate
      set Q-farm (farm-area-total * pumping-cap * IWA * 1000 / growing-season)                ;; [m3/d]
    ]
    if decision = "defect"
    [
      let max-breach (farm-area-total * (1 - pumping-cap))                                    ;; each breach will be something in between irrigating the whole farm or a portion of it, [ha]
      set breach boldness * max-breach                                                        ;; a breach proportional to the farmer's boldness, [ha]
      set irrigated-area (farm-area-total * pumping-cap + breach)                             ;; the area that the farmer has chosen to irrigate
      set Q-farm ((farm-area-total * pumping-cap + breach) * IWA * 1000 / growing-season)     ;; [m3/d]
    ]
    set Qwell Q-farm
  ]
end

to CALCULATE-PUMPING-LIFT
  ask farmers [set Hlift (Hground - H)]
end

to CALCULATE-FARMER-COLOURS
  ask farmers [ifelse decision = "defect" [set color red][set color blue]]
end

to UPDATE-DEFECTOR-AND-COOPERATOR-LISTS
  set defectors farmers with [decision = "defect"]
  set cooperators farmers with [decision = "cooperate"]
end

to UPDATE-CUMMULATIVE-INDICATORS
  ask farmers
  [
    set cummulative-wealth cummulative-wealth + E-score
  ]
end

to DECIDE-DEFECT-OR-NOT
  ask farmers
  [
    if boldness = 0 [set decision "cooperate"]
    if boldness = 1 [set decision "defect"]
    if (boldness != 1) and (boldness != 0)
    [
      ifelse boldness >= (random 11 / 10) [set decision "defect"][set decision "cooperate"]
    ]
  ]
end

to DECIDE-PUNISH-OR-NOT
  ask defectors
  [
    let defectors-name self
    ask neighbors-list
    [
      ifelse vengefulness >= random 11 / 10                                ;; punish the defection
      [
        set times-punished-others times-punished-others + 1                ;; increase the punisher's punishing counter
        if metanorm? = true [set bad-monitor? false]                       ;; this agent punished the defection
        ask defectors-name                                                 ;; and for this specific defector
        [
          set times-caught-defecting times-caught-defecting + 1            ;; increase the defectors punishment counter
        ]
      ]
      [
        if metanorm? = true [set bad-monitor? true]                         ;; this agent did not punish the defection
      ]
    ]
    if metanorm? = true [set neighbours-that-punished count neighbors-list with [bad-monitor? = false]]
  ]
end

to CLEAR-SOCIAL-COUNTERS
  ask farmers [set times-punished-others 0 set times-caught-defecting 0 set neighbours-that-punished 0]
end

to EVOLVE-STRATEGY

  ifelse memory? = false

    [

      ask farmers with [voluntary-compliance? = false and rule-breaker? = false]
      [
        let neighbours-list-and-me (turtle-set self neighbors-list)
        set boldness [boldness] of max-one-of neighbours-list-and-me [score]
        set vengefulness [vengefulness] of max-one-of neighbours-list-and-me [score]       ;; copy the strategy of my wealthiest neighbor

        if random-float 1 < mutation-probability
        [
          ifelse random-float 1 < 0.5                                                      ;; an equal chance of flipping my boldness or vengefulness
          [set boldness (random 11 / 10)]                                                  ;; mutate boldness
          [set vengefulness (random 11 / 10)]                                              ;; or mutate vengefulness
        ]
      ]
    ]

    [
      ask farmers with [voluntary-compliance? = false and rule-breaker? = false]
      [
        let neighbours-list-and-me (turtle-set self neighbors-list)
        set boldness [boldness] of max-one-of neighbours-list-and-me [score-accumulated]
        set vengefulness [vengefulness] of max-one-of neighbours-list-and-me [score-accumulated]       ;; copy the strategy of my wealthiest neighbor

        if random-float 1 < mutation-probability
        [
          ifelse random-float 1 < 0.5                                                      ;; an equal chance of flipping my boldness or vengefulness
          [set boldness (random 11 / 10)]                                                  ;; mutate boldness
          [set vengefulness (random 11 / 10)]                                              ;; or mutate vengefulness
        ]
      ]
    ]
end


to UPDATE-FARMER-COMPLIANCE-RISK
  ask farmers [if times-caught-defecting > 0 [set breach-severity breach set times-reported times-caught-defecting]]
end

to CLASSIFY-OVERALL-SEVERITY
  set agents-sorted-by-overall-severity sort-on [(- overall-severity-number)] farmers-inspected-this-turn with [decision = "defect"]
  let length-of-list length agents-sorted-by-overall-severity

  if length-of-list > 3
  [
    let tier-1-ceiling int (length-of-list / 3)

    let tier-2-floor tier-1-ceiling + 1
    let tier-2-ceiling int ((2 * length-of-list) / 3)

    let tier-3-floor tier-2-ceiling + 1
    let tier-3-ceiling (length-of-list - 1)

    set tier-1 sublist agents-sorted-by-overall-severity 0 tier-1-ceiling
    set tier-2 sublist agents-sorted-by-overall-severity tier-2-floor tier-2-ceiling
    set tier-3 sublist agents-sorted-by-overall-severity tier-3-floor tier-3-ceiling

    foreach tier-1 [ask ? [set overall-severity-category "tier 1"] ]
    foreach tier-2 [ask ? [set overall-severity-category "tier 2"] ]
    foreach tier-3 [ask ? [set overall-severity-category "tier 3"] ]
  ]

  if length-of-list = 3
  [
    set tier-1 item 0 agents-sorted-by-overall-severity
    set tier-2 item 1 agents-sorted-by-overall-severity
    set tier-3 item 2 agents-sorted-by-overall-severity
  ]

  if length-of-list = 2
  [
    set tier-1 item 0 agents-sorted-by-overall-severity
    set tier-2 item 1 agents-sorted-by-overall-severity
  ]

  if length-of-list = 1
  [
    set tier-1 item 0 agents-sorted-by-overall-severity
  ]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; SCORE FUNCTIONS ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CALCULATE THE ECONOMIC SCORE
to CALCULATE-E-SCORE-WINTER
  ask farmers
  [
    set IN (crop-price * crop-yield)
    set VC-pumping (energy-price * 9.8 * IWA * Hlift) / (pump-efficiency)               ;; pump efficiency equal for everyone
    set VC-other variable-costs-other
    set VC (VC-pumping + VC-other)
    set E-score-winter (IN - VC) * irrigated-area
  ]
end

to CALCULATE-E-SCORE-SUMMER
  ask farmers
  [
    set IN (crop-price * crop-yield)
    set VC-pumping (energy-price * 9.8 * IWA * Hlift) / (pump-efficiency)               ;; pump efficiency equal for everyone
    set VC-other variable-costs-other
    set VC (VC-pumping + VC-other)
    set E-score-summer (IN - VC) * irrigated-area
  ]
end

to CALCULATE-E-SCORE
  ask farmers [set E-score E-score-winter + E-score-summer]
end

;; CALCULATE THE INSTITUTIONAL SCORE
to CALCULATE-I-SCORE
  if monitoring-style = "flat"
  [
    set monitoring-percent  max-monitoring-capacity
  ]

  if monitoring-style = "adaptive"
  [
    set monitoring-percent  ((1 - compliance-level) ^ risk-regulation-factor) ifelse monitoring-percent >= max-monitoring-capacity [set monitoring-percent max-monitoring-capacity][]
  ]

  ifelse graduated-sanctions? = true
  [

    if enforcement-strategy = "random"
    [

      set farmers-inspected-this-turn n-of (monitoring-percent  * num-farmers) farmers

      ask farmers-inspected-this-turn with [decision = "defect"]
      [
        set caught-this-year? true
        if caught-this-year? = true and caught-last-year? = true [set repetition-severity repetition-severity + 1]
        set overall-severity-number breach-severity * repetition-severity
      ]

      ask farmers-inspected-this-turn with [decision = "cooperate"]
      [
        set caught-this-year? false
        if repetition-severity = 1 [set repetition-severity 0]
        if repetition-severity > 1 [set repetition-severity repetition-severity - 1]
        set I-score 1
        set caught-last-year? caught-this-year?
      ]

      CLASSIFY-OVERALL-SEVERITY

      ask farmers-inspected-this-turn with [decision = "defect"]
      [
        if overall-severity-category = "tier 1" [set graduated-fine 1.00]
        if overall-severity-category = "tier 2" [set graduated-fine 0.25]
        if overall-severity-category = "tier 3" [set graduated-fine 0.10]
        set I-score (1 - graduated-fine)
        set caught-last-year? caught-this-year?
      ]

    ]

    if enforcement-strategy = "risk-based"
    [

      set farmers-inspected-this-turn max-n-of (monitoring-percent  * num-farmers) farmers [breach-severity * times-reported]

      ask farmers-inspected-this-turn with [decision = "defect"]
      [
        set caught-this-year? true
        if caught-this-year? = true and caught-last-year? = true [set repetition-severity repetition-severity + 1]
        set overall-severity-number breach-severity * repetition-severity
      ]

      ask farmers-inspected-this-turn with [decision = "cooperate"]
      [
        set caught-this-year? false
        if repetition-severity = 1 [set repetition-severity 0]
        if repetition-severity > 1 [set repetition-severity repetition-severity - 1]
        set I-score 1
        set caught-last-year? caught-this-year?
      ]

      CLASSIFY-OVERALL-SEVERITY

      ask farmers-inspected-this-turn with [decision = "defect"]
      [
        if overall-severity-category = "tier 1" [set graduated-fine 0.1]
        if overall-severity-category = "tier 2" [set graduated-fine 0.25]
        if overall-severity-category = "tier 3" [set graduated-fine 1.00]
        set I-score (1 - graduated-fine)
        set caught-last-year? caught-this-year?
      ]

    ]

  ]

  [
    if enforcement-strategy = "random" [ask n-of (monitoring-percent  * num-farmers) farmers [ifelse decision = "defect" [set I-score (1 - fine-magnitude)][set I-score 1]]]
    if enforcement-strategy = "risk-based" [ask max-n-of (monitoring-percent  * num-farmers) farmers [breach-severity * times-reported] [ifelse decision = "defect" [set I-score (1 - fine-magnitude)][set I-score 1]]]
  ]
end

;; CALCULATE THE SOCIAL SCORE
to CALCULATE-S-SCORE
  ask farmers
  [
    if S-enforcement-cost > 0            ;; enforcement is perceived as a social cost
    [
      if times-punished-others = 0 and (times-caught-defecting + neighbours-that-punished) != 0  [set S-score (1 - S-reputation) ^ (times-caught-defecting + neighbours-that-punished)]
      if times-punished-others != 0 and (times-caught-defecting + neighbours-that-punished) = 0  [set S-score (1 - S-enforcement-cost)  ^ times-punished-others]
      if times-punished-others != 0 and (times-caught-defecting + neighbours-that-punished) != 0  [set S-score ((1 - S-reputation) ^ (times-caught-defecting + neighbours-that-punished)) * ((1 - S-enforcement-cost) ^ times-punished-others)]
    ]
    if S-enforcement-cost <= 0          ;; enforcement is perceived as a social reward
    [
      if times-punished-others = 0 and (times-caught-defecting + neighbours-that-punished) != 0  [set S-score (1 - S-reputation) ^ (times-caught-defecting + neighbours-that-punished)]
      if times-punished-others != 0 and (times-caught-defecting + neighbours-that-punished) = 0  [set S-score (1 + (S-enforcement-cost * -1)) ^ times-punished-others]
      if times-punished-others != 0 and (times-caught-defecting + neighbours-that-punished) != 0  [set S-score ((1 - S-reputation) ^ (times-caught-defecting + neighbours-that-punished)) * (1 + (S-enforcement-cost * -1)) ^ times-punished-others]
    ]
  ]
end

;; CALCULATE THE TOTAL SCORE
to UPDATE-SCORES
  ifelse economics = true [CALCULATE-E-SCORE][ask farmers [set E-score 1]]
  ifelse institution = true [CALCULATE-I-SCORE][ask farmers [set I-score 1]]
  ifelse social = true [CALCULATE-S-SCORE][ask farmers [set S-score 1]]
  ask farmers
  [
    set score E-score * I-score * S-score
    UPDATE-SCORE-LIST
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; SET CROP ECONOMICS ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to SET-SUMMER-CROP-ECONOMICS
  ;if economy? = "Central Valley: Almonds(S)"
  ;[
  ;  set crop-price 1.5
  ;  set IWA 13.2
  ;  set crop-yield 6931
  ;  set energy-price 0.15
  ;  set variable-costs-other 4750
  ;]

  if economy? = "Australia: Cotton(S), Vetch(W)"
  [
    set crop-price 580
    set IWA 9.5
    set crop-yield 10.5
    set energy-price 0.2
    set variable-costs-other 2825
  ]

  ;if economy? = "Punjab: Rice(S), Wheat(W)"
  ;[
    ;;summer rice
   ; set crop-price 0.11
    ;set IWA 13.5
    ;set crop-yield 6960
    ;set energy-price 0.016
    ;set variable-costs-other 322
  ;]

    if economy? = "Morocco-Tadla: Onion(S), Sugarbeet(W)"
    [
    set crop-price 1300
    set IWA 5.6
    set crop-yield 30
    set energy-price 1.254
    set variable-costs-other 19116
    ]
end

to SET-WINTER-CROP-ECONOMICS
  ;if economy? = "Central Valley: Almonds(S)"
  ;[
  ;  set crop-price 0
  ;  set IWA 0
  ;  set crop-yield 0
  ;  set energy-price 0
  ;  set variable-costs-other 0
 ; ]

  if economy? = "Australia: Cotton(S), Vetch(W)"
  [
    set crop-price 0
    set IWA 1.4
    set crop-yield 0
    set energy-price 0.2
    set variable-costs-other 188
  ]

 ; if economy? = "Punjab: Rice(S), Wheat(W)"
 ; [
    ;;winter wheat
 ;   set crop-price 0.12
 ;   set IWA 4.1
 ;   set crop-yield 5525
 ;   set energy-price 0.016
 ;   set variable-costs-other 189
 ; ]

    if economy? = "Morocco-Tadla: Onion(S), Sugarbeet(W)"
    [
    set crop-price 400
    set IWA 2.9
    set crop-yield 84
    set energy-price 1.083
    set variable-costs-other 11089
    ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; LORENZ AND GINI ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to UPDATE-LORENZ-AND-GINI
  set sorted-wealths sort [E-score] of farmers
  let total-wealth sum sorted-wealths
  let wealth-sum-so-far 0
  let index 0
  set gini-index-reserve 0
  set lorenz-points []

  ;; now actually plot the Lorenz curve -- along the way, we also
  ;; calculate the Gini index.
  ;; (see the Info tab for a description of the curve and measure)
  repeat num-farmers [
    set wealth-sum-so-far (wealth-sum-so-far + item index sorted-wealths)
    set lorenz-points lput ((wealth-sum-so-far / total-wealth) * 100) lorenz-points
    set index (index + 1)
    set gini-index-reserve
    gini-index-reserve +
    (index / num-farmers) -
    (wealth-sum-so-far / total-wealth)
  ]
end


to UPDATE-COMPLIANCE-LEVEL
  set compliance-level (count farmers with [decision = "cooperate"] / num-farmers)
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;   NORM-BREAKING MECHANISM #1: UNCONDITIONAL RULE-BREAKERS   ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to COUNT-FARMERS-WITH-RULE-BREAKER
  set number-of-farmers-with-rule-breaker count farmers with [rule-breaker? = true]
end


to UPDATE-RULE-BREAKERS
  let new-number-of-farmers-with-rule-breaker (rule-breaker-level * num-farmers)
  let difference abs (new-number-of-farmers-with-rule-breaker - number-of-farmers-with-rule-breaker)

  if (new-number-of-farmers-with-rule-breaker > number-of-farmers-with-rule-breaker)        ;; the voluntary-compliance increases
  [
    ask n-of difference farmers with [rule-breaker? = false][set rule-breaker? true set boldness 1 set vengefulness 0]
  ]

  if (new-number-of-farmers-with-rule-breaker < number-of-farmers-with-rule-breaker)        ;; the voluntary-compliance decreases
  [
    ask n-of difference farmers with [rule-breaker? = true][set rule-breaker? false]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;   NORM-ENFORCING MECHANISM #1: VOLUNTARY COMPLIANCE   ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to COUNT-FARMERS-WITH-VOLUNTARY-COMPLIANCE
  set number-of-farmers-with-voluntary-compliance count farmers with [voluntary-compliance? = true]
end


to UPDATE-VOLUNTARY-COMPLIANCE
  let new-number-of-farmers-with-voluntary-compliance (voluntary-compliance-level * num-farmers)
  let difference abs (new-number-of-farmers-with-voluntary-compliance - number-of-farmers-with-voluntary-compliance)

  if (new-number-of-farmers-with-voluntary-compliance > number-of-farmers-with-voluntary-compliance)        ;; the voluntary-compliance increases
  [
    ask n-of difference farmers with [voluntary-compliance? = false][set voluntary-compliance? true set boldness 0 set vengefulness 1]
  ]

  if (new-number-of-farmers-with-voluntary-compliance < number-of-farmers-with-voluntary-compliance)        ;; the voluntary-compliance decreases
  [
    ask n-of difference farmers with [voluntary-compliance? = true][set voluntary-compliance? false]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;::::
;;;;;;;;;;;;;;   NORM-ENFORCING MECHANISM # 2: META-PUNISH   ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INCORPORATES AN ADDITIONAL LAYER OF PUNISHMENT I.E. TO THOSE AGENTS THAT DO NOT ENFORCE THE NORM
to META-PUNISH
  ask farmers with [being-bad-monitor? = true]
  [
    let bad-monitors-name self
    set seen-being-bad-monitor? false

    let probability-of-seeing-bad-monitor (random 11 / 10)
    let probability-of-punishing-bad-monitor (random 11 / 10)

    ask neighbors-list
    [

      if (probability-of-seeing-bad-monitor <= S) and (probability-of-punishing-bad-monitor <= vengefulness)
      [
        set score score
        set this-turns-reputation this-turns-reputation + 1
        ask bad-monitors-name
        [
          set score score
          set seen-being-bad-monitor? true
          set this-turns-reputation this-turns-reputation - 1]
      ]
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;   NORM-ENFORCING MECHANISM # 3: MEMORY   ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; KEEPS TRACK OF THE SCORE HISTORY OF EACH AGENT, WHICH IS USED IN THE 'MEMORY' MECHANISM
to UPDATE-SCORE-LIST
  ask farmers
  [
    if length score-history-list < memory-length                               ;; length of score history is shorter than the memory = simulation is starting
    [
      set score-history-list lput score score-history-list
    ]

    if length score-history-list = memory-length                               ;; length of score history is equal to memory = remove oldest and add newest score
    [
      set score-history-list but-first score-history-list
      set score-history-list lput score score-history-list
    ]

    if length score-history-list > memory-length
    [
      set score-history-list sublist score-history-list (length score-history-list - memory-length) (length score-history-list - 1)
      set score-history-list lput score score-history-list
    ]

    set score-accumulated reduce + score-history-list
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;   NORM-ENFORCING MECHANISM #4: REPUTATION   ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; KEEPS TRACK OF THE REPUTATION HISTORY OF EACH AGENT, WHICH IS USED IN THE 'REPUTATION' MECHANISM
to UPDATE-REPUTATION-LIST
  ask farmers
  [
    if length reputation-history-list < memory-reputation                               ;; length of score history is shorter than the memory = simulation is starting
    [
      set reputation-history-list lput this-turns-reputation reputation-history-list
    ]

    if length reputation-history-list = memory-reputation                               ;; length of score history is equal to memory = remove oldest and add newest score
    [
      set reputation-history-list but-first reputation-history-list
      set reputation-history-list lput this-turns-reputation reputation-history-list
    ]

    if length reputation-history-list > memory-reputation
    [
      set reputation-history-list sublist reputation-history-list (length reputation-history-list - memory-reputation) (length reputation-history-list - 1)
      set reputation-history-list lput this-turns-reputation reputation-history-list
    ]

    set reputation-accumulated reduce + reputation-history-list
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GW - PROCESSES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to solve-once
  calculate-source-term                       ;; comprises the sources (wells, ET) and sinks (injection, recharge)
  calculate-RHS                               ;; obtain the RHS of each equation
  build-matrix-C                              ;; these only need to be calculated at the beggining of each iteration
  reduce-matrix-C                             ;; eliminate the sourceterms corresponding to no-flow and fixed-head cells
  solve-system-of-equations                   ;; solve the system of equations AxB=C
  list-of-active-cells                        ;; lists the cells to be updated with new heads
  extract-Hinew                               ;; the new heads are mapped into the Hinew variable of the grid
  update-heads-to-grid                        ;; H <-- Hinew / the iteration has ended so we update the H values for the iterations corresponding to the next timestep
end


to calculate-source-term

  ifelse (sine-recharge-multiplier? = true and solver = "transient") [calculate-multiplier-sine-recharge][set sine-recharge 1]
  ifelse (sine-well-multiplier? = true and solver = "transient") [calculate-multiplier-sine-wells][set sine-well 1]

  if solver = "steady-state"                                      ;; IMPORTANT NOTE: when using ET, DRAIN and RIV (head-dependent) cells, set initial conditions using a long-term transient simulation
  [
  ]

  if solver = "transient"                                         ;; recalculate the ET and DRAIN terms before each iteration
  [
    ask patches with [ET? = true][calculate-ET-discharge]
    ask patches with [DRAIN? = true][calculate-DRAIN-discharge]
    ask patches with [RIV? = true][calculate-RIV-term]
  ]

  ;;FOR THE CELLS THAT ARE NOT FARMER WELLS I.E. TOWN WELLS PUMP CONSTANTLY THROUGHOUT THE YEAR
  ask patches with [interior-node? = true]
  [
    set SourceTerm ((Qwell - Qinjection) * sine-well / (delta ^ 2)) + (- Recharge * sine-recharge) + (ET) + (DRAIN / (delta ^ 2)) + (RIV / (delta ^ 2))      ;; units consistent [L3/L2*T] ~ [L/T]

                                                                                                                                                             ;; look at neighbours for fixed-head boundary, we know the heads here and therefore move this term to the RHS

    if [fixed-head?] of N-neighbour = true [set SourceTerm (SourceTerm - ([H] of N-neighbour) * (Ncof))]
    if [fixed-head?] of S-neighbour = true [set SourceTerm (SourceTerm - ([H] of S-neighbour) * (Scof))]
    if [fixed-head?] of E-neighbour = true [set SourceTerm (SourceTerm - ([H] of E-neighbour) * (Ecof))]
    if [fixed-head?] of W-neighbour = true [set SourceTerm (SourceTerm - ([H] of W-neighbour) * (Wcof))]
  ]
end

to calculate-RHS
  ask patches with [interior-node? = true]
  [set RHS (-1 * S-patch) / delta-t * (H) + SourceTerm]
end


to build-matrix-C
  set C matrix:make-constant (number-of-unknowns) 1 0
  ask patches with [interior-node? = true]
  [
    let my-position ([patch-number-tag] of self - 1)                ;; NOTE: indexing of the matrix extension starts at 0, not 1
    matrix:set C (my-position) 0 [RHS] of self                      ;; set Q term of the RHS of the equation
  ]
end

to solve-system-of-equations
  set solution-vector matrix:times inverse-A C
end

to extract-Hinew
  let i 0
  while [i <= length active-cells-remap - 1]
  [
    ask patches with [patch-number-tag = (item i active-cells-remap)][set Hinew matrix:get (solution-vector) i 0]
    set i i + 1
  ]
end

to update-heads-to-grid
  ask patches with [interior-node? = true and no-flow? = false and fixed-head? = false][set H Hinew]                    ;; replace the old head value with the new one
end

to calculate-ET-discharge                                                                                             ;; recalculates the ET term at each iteration
  ask patches with [ET? = true]
  [
    if H > ET-land-surface-elevation-patch [set ET ET-max-rate-patch]                                                  ;; if head is greater than land surface elevation

    if H >= (ET-land-surface-elevation-patch - ET-extinction-depth-patch)                                              ;; if head is between land surface and extinction depth
    and H <= (ET-land-surface-elevation-patch)
    [set ET ET-max-rate-patch * (H - (ET-land-surface-elevation-patch - ET-extinction-depth-patch)) / ET-extinction-depth-patch]   ;; a linear relationship between the max rate and zero

    if H < (ET-land-surface-elevation-patch - ET-extinction-depth-patch) [set ET 0]                                    ;; if head is lower than extinction depth
  ]

end

to calculate-DRAIN-discharge                                                                                          ;; recalculates the springflow at each iteration NOTE: this values is in m3/day, needs to be divided by the cell area in the sourceterm equation
  ask patches with [DRAIN? = true]
  [
    if H > DRAIN-elevation-patch [set DRAIN DRAIN-conductance-patch * (H - DRAIN-elevation-patch)]
    if H <= DRAIN-elevation-patch [set DRAIN 0]
  ]
end

to calculate-RIV-term                                                                                         ;; recalculates the springflow at each iteration NOTE: this values is in m3/day, needs to be divided by the cell area in the sourceterm equation
  ask patches with [RIV? = true]
  [
    if H > RIV-elevation-patch [set RIV RIV-conductance-patch * (H - RIV-elevation-patch)]
    if H <= RIV-elevation-patch and H > RIV-bottom-patch [set RIV RIV-conductance-patch * (RIV-elevation-patch - H) * -1]
    if H <= RIV-elevation-patch and H < RIV-bottom-patch [set RIV RIV-conductance-patch * (RIV-elevation-patch - RIV-bottom-patch) * -1]
  ]
end

to calculate-multiplier-sine-recharge                                            ;; assumes that recharge occurs in (southern hemisphere) winter
  let B-recharge (2 * pi / 365)
  let C-recharge 81.75
  set sine-recharge sin (B-recharge * (ticks - C-recharge) * 180 / pi)         ;; note that the sine function here is in degrees, not in radians
  if sine-recharge < 0 [set sine-recharge 0]
end


to calculate-multiplier-sine-wells                                               ;; assumes that pumping occurs in (southern hemisphere) summer
  let B-well (2 * pi / 365)
  let C-well -81.75
  set sine-well sin (B-well * (ticks - C-well) * 180 / pi)                     ;; note that the sine function here is in degrees, not in radians
  if sine-well < 0 [set sine-well 0]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; REMOVE INACTIVE CELLS FROM CALCULATIONS / SPEEDS UP SOLUTION ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to remove-inactive-cells-from-A-matrix
  list-inactive-cells
  define-rows-to-remove
  remove-rows
  transform-A-from-row-list-to-matrix
  remove-columns
  transform-A-from-column-list-to-matrix
  set A A-reduced
end

to remove-rows
  transform-A-to-row-list
  remove-rows-from-A
end

to remove-columns
  transform-A-to-column-list
  remove-columns-from-A
end

to reduce-matrix-C
  transform-C-to-row-list
  remove-rows-from-C
  transform-C-from-row-list-to-matrix
  set C C-reduced
end

to list-inactive-cells                                                              ;; these are either no-flow or fixed head cells in the model. We need to remove these from the calculations
  ask patches with [interior-node? = true]
  [
    set no-flow-cells-list (list [patch-number-tag] of patches with [no-flow? = true and interior-node? = true])
    set fixed-head-cells-list (list [patch-number-tag] of patches with [fixed-head? = true and interior-node? = true])
    set remove-cells-list sentence no-flow-cells-list fixed-head-cells-list
    set remove-cells-list sort sentence item 0 remove-cells-list item 1 remove-cells-list
  ]
end

to define-rows-to-remove                                                            ;; creates a list containing the index of patches that we will remove. Note that the indexing begints at 0. It considers the fact that the remove-item function removes one row at a time and the indexes change during this process
  let i 0
  set remove-cells-list-corrected []
  while [i <= (length remove-cells-list - 1) ]
  [
    let index-of-cell-to-remove (item i remove-cells-list) - i - 1
    set remove-cells-list-corrected lput index-of-cell-to-remove remove-cells-list-corrected
    set i i + 1
  ]
end

to transform-A-to-row-list                                                         ;; transforms matrix A into list form (rows)
  set A-row-list matrix:to-row-list A
end

to remove-rows-from-A                                                              ;; removes the rows one by one
  let i 0
  set A-row-reduced-list A-row-list
  while [i <= (length remove-cells-list - 1) ]
  [
    let index-of-cell-to-remove (item i remove-cells-list-corrected)
    set A-row-reduced-list remove-item index-of-cell-to-remove A-row-reduced-list
    set i i + 1
  ]
end

to transform-A-from-row-list-to-matrix                                              ;; back to matrix form
  set A-row-reduced-matrix matrix:from-row-list A-row-reduced-list
end

to transform-A-to-column-list                                                      ;; transforms matrix A into list form (columns)
  set A-column-list matrix:to-column-list A-row-reduced-matrix
end

to remove-columns-from-A                                                           ;; removes the rows one by one
  let i 0
  set A-column-reduced-list A-column-list
  while [i <= (length remove-cells-list - 1) ]
  [
    let index-of-cell-to-remove (item i remove-cells-list-corrected)
    set A-column-reduced-list remove-item index-of-cell-to-remove A-column-reduced-list
    set i i + 1
  ]
end

to transform-A-from-column-list-to-matrix                                              ;; back to matrix form
  set A-reduced matrix:from-column-list A-column-reduced-list
end

to transform-C-to-row-list
  set C-row-list matrix:to-row-list C
end

to remove-rows-from-C
  let i 0
  set C-row-reduced-list C-row-list
  while [i <= (length remove-cells-list - 1) ]
  [
    let index-of-cell-to-remove (item i remove-cells-list-corrected)
    set C-row-reduced-list remove-item index-of-cell-to-remove C-row-reduced-list
    set i i + 1
  ]
end

to transform-C-from-row-list-to-matrix
  set C-reduced matrix:from-row-list C-row-reduced-list
end

to list-of-active-cells
  set active-cells-remap (list [patch-number-tag] of patches with [no-flow? = false and fixed-head? = false and interior-node? = true])
  set active-cells-remap sort item 0 active-cells-remap
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; I/O AND BATCH EXPERIMENTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; WRITE HEADS TO FILE ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to write-output-heads
  file-delete "heads.txt"
  file-open "heads.txt"
  let j 1
  while [j <= N]
  [
    let i 1
    while [i <= M]
    [
      if i <= M [ask patch i j [file-write H]]
      if i = M [ask patch i j [file-print ""]]
      set i i + 1
    ]
    set j j + 1
  ]
  file-close
  file-flush
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GW MODEL - POINT AND CLICK ACTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; SET BCs / POINT-AND-CLICK ON THE INTERFACE ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to change-to-fixed-head
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
      [
        set pcolor blue
        set fixed-head? true
        set no-flow? false
        set H fixed-head-value-new
        set K-patch K
        ifelse aquifer-type = "confined"
        [set T-patch (K-patch * aquifer-thickness)]       ;; sets transmissivity and storage for confined conditions
        [set T-patch (K-patch * H)]                      ;; sets transmissivity and storage for unconfined conditions
        set plabel-color black
        set plabel H
      ]
    ]
end

to change-to-no-flow
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
      [
        set pcolor black
        set fixed-head? false
        set no-flow? true
      ]
    ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; SET HYDRAULIC PARAMETERS / POINT-AND-CLICK ON THE INTERFACE ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-K
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
      [
        set K-patch K-input
        set plabel-color black
        set plabel K-patch
      ]
    ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; SET DRAIN / POINT-AND-CLICK ON THE INTERFACE ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-DRAIN-patch
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
      [
        set DRAIN? true
        set DRAIN-elevation-patch DRAIN-elevation
        set DRAIN-conductance-patch DRAIN-conductance
        set plabel-color black
        set pcolor magenta
        set plabel DRAIN-conductance-patch
      ]
    ]
end

to clear-DRAIN-patches-all
  ask patches with [DRAIN? = true]
  [
    set DRAIN? false
    set DRAIN 0
    set DRAIN-elevation-patch 0
    set DRAIN-conductance-patch 0
    set pcolor white
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; SET RIV / POINT-AND-CLICK ON THE INTERFACE ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-RIV-patch
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
      [
        set RIV? true
        set RIV-elevation-patch RIV-elevation
        set RIV-bottom-patch RIV-bottom
        set RIV-conductance-patch RIV-conductance
        set plabel-color black
        set pcolor cyan
        set plabel RIV-elevation-patch
      ]
    ]
end

to clear-RIV-patches-all
  ask patches with [RIV? = true]
  [
    set RIV? false
    set RIV 0
    set RIV-elevation-patch 0
    set RIV-bottom-patch 0
    set RIV-conductance-patch 0
    set pcolor white
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; SET ET / POINT-AND-CLICK ON THE INTERFACE ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-ET-patch
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
      [
        set ET? true
        set ET-max-rate-patch ET-max-rate
        set ET-extinction-depth-patch ET-extinction-depth
        set ET-land-surface-elevation-patch ET-land-surface-elevation
        set plabel-color black
        set pcolor brown
        set plabel ET-max-rate-patch
      ]
    ]
end

to clear-ET-patches-all
  ask patches with [ET? = true]
  [
    set ET? false
    set ET-max-rate 0
    set ET-extinction-depth 0
    set ET-land-surface-elevation 0
    set pcolor white
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; SET FIXED FLUX — POINT-AND-CLICK ON THE INTERFACE ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to place-fixed-flux
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
        [
          set pcolor magenta
          set fixed-flux? true
          set injection? true
          set Qinjection Qfixed-flux
          set plabel-color black
          set plabel Qinjection
        ]
    ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; SET WELLS — POINT-AND-CLICK ON THE INTERFACE ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to place-pumping-well
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
      [
        set pcolor red
        set well? true
        set Qwell Qwell-input
        ;set screen-level screen-level-input
        set plabel-color black
        set plabel Qwell
      ]
    ]
end

to place-injection-well
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
        [
          set pcolor blue
          set injection? true
          set Qinjection Qinjection-input
          set plabel-color black
          set plabel Qinjection
        ]
    ]
end

;; the following routines allow clearing the view from stresses and resetting them using the interface buttons

to hide-stresses-patches
  ask patches with [well? = true or injection? = true]
  [
    if well? = true [set pcolor white set plabel-color red]
    if injection? = true [set pcolor white set plabel-color blue]
  ]
end

to hide-stresses-labels
  ask patches with [well? = true or injection? = true]
  [
    if well? = true [set plabel ""]
    if injection? = true [set plabel ""]
  ]
end

to clear-stresses
  ask patches with [well? = true or injection? = true]
  [
    if well? = true [set Qwell 0 hide-stresses-patches hide-stresses-labels set well? false]
    if injection? = true [set Qinjection 0 hide-stresses-patches hide-stresses-labels set injection? false]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; SET RECHARGE / POINT-AND-CLICK ON THE INTERFACE AND ALL CELLS ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to set-recharge-all-patches
  ask patches with [interior-node? = true][set Recharge areal-recharge]
end

to set-recharge-single-patch
  if mouse-down?     ;; reports true or false to indicate whether mouse button is down
    [
      ask patch mouse-xcor mouse-ycor
      [
        set Recharge areal-recharge
        set plabel-color black
        set plabel Recharge
      ]
    ]
end

to clear-recharge
  ask patches with [interior-node? = true][set Recharge 0]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MODEL VIEWS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to refresh-view
  if view = "none" [ask patches with [interior-node? = true] [set plabel-color 57]]
  if view = "patch numbering" [ask patches with [interior-node? = true] [set plabel precision patch-number-tag 1 set plabel-color black]]
  if view = "boundary conditions" [view-bc ask patches with [no-flow? = true][set pcolor black] ask patches with [fixed-head? = true][set pcolor blue]ask patches with [RIV? = true][set pcolor cyan] ask patches with [well? = true][set pcolor red] ask patches with [injection? = true][set pcolor blue]]
  if view = "wells" [ask patches with [interior-node? = true][set plabel ""] ask patches with [well? = true][set plabel-color black set plabel "W"] ask patches with [injection? = true][set plabel-color black set plabel "R"]]
  if view = "T (values)" [ask patches [set plabel "" set plabel precision T-patch 0 set plabel-color black]]
  if view = "K (values)" [ask patches [set plabel "" set plabel precision K-patch 0 set plabel-color black]]
  if view = "K (contours)" [setup-maxmin-K ask patches with [interior-node? = true and no-flow? = false and ET? = false and fixed-head? = false and DRAIN? = false and RIV? = false] [set pcolor scale-color brown K-patch min-K max-K set plabel precision K-patch 0 set plabel-color black] ask patches with [no-flow? = true][set pcolor black]]
  if view = "S (values)" [ask patches [set plabel "" set plabel precision S-patch 5 set plabel-color black]]

  if view = "heads (values)" [ask patches [set plabel precision H 0 set plabel-color black]]
  if view = "heads (contours)" [setup-maxmin-heads ask patches with [interior-node? = true and no-flow? = false and ET? = false and fixed-head? = false and DRAIN? = false and RIV? = false] [setup-maxmin-heads set pcolor scale-color gray H min-head max-head set plabel precision H 0 set plabel-color black] ask patches with [no-flow? = true][set pcolor black] ask patches with [ET? = true][set plabel precision H 0 set plabel-color black]]

  if view = "areal recharge" [ask patches [set plabel "" set plabel precision Recharge 4 set plabel-color black]]

  if view = "ET" [ask patches [set plabel "" ask patches with [ET? = true][set plabel precision Recharge 4 set plabel-color black set pcolor green]]]

  if view = "DRAIN conductance" [ask patches with [DRAIN? = true][set plabel DRAIN-conductance-patch]]
  if view = "DRAIN flow [m3/d] and head values" [ask patches with [DRAIN? = true][set plabel precision DRAIN 0] ask patches with [DRAIN? = false][set plabel precision H 0 set plabel-color black]]                    ;; cells that are not drains show the head value
  if view = "DRAIN flow [L/s] and head values" [ask patches with [DRAIN? = true][set plabel precision (DRAIN * 1000 / 86400) 0] ask patches with [DRAIN? = false][set plabel precision H 0 set plabel-color black]]    ;; cells that are not drains show the head value

  if view = "RIV flow [m3/d] and head values" [ask patches with [RIV? = true][set plabel precision RIV 0 if RIV < 0 [set pcolor red] if RIV >= 0 [set pcolor cyan]] ask patches with [RIV? = false][set plabel precision H 0 set plabel-color black]]                    ;; cells that are not drains show the head value
  if view = "RIV flow [L/s] and head values" [ask patches with [RIV? = true][set plabel precision (RIV * 1000 / 86400) 0 if RIV < 0 [set pcolor red] if RIV >= 0 [set pcolor cyan]] ask patches with [RIV? = false][set plabel precision H 0 set plabel-color black]]    ;; cells that are not drains show the head value
  if view = "RIV flow [L/s] and drawdowns" [ask patches with [RIV? = true][set plabel precision (RIV * 1000 / 86400) 0 if RIV < 0 [set pcolor red] if RIV >= 0 [set pcolor cyan]] ask patches with [RIV? = false][set plabel precision (Hinitial - H) 0 set plabel-color black]]    ;; cells that are not drains show the head value

  if view = "RIV stage and head values" [ask patches with [DRAIN? = true][set plabel precision RIV-elevation-patch 0] ask patches with [DRAIN? = false][set plabel precision H 0 set plabel-color black]]      ;; cells that are not drains show the head value
  if view = "RIV bottom and head values" [ask patches with [DRAIN? = true][set plabel precision RIV-bottom-patch 0] ask patches with [DRAIN? = false][set plabel precision H 0 set plabel-color black]]        ;; cells that are not drains show the head value
  if view = "RIV conductance" [ask patches with [DRAIN? = true][set plabel precision RIV-conductance-patch 0] ask patches with [DRAIN? = false][set plabel ""]]                                                ;; cells that are not drains are blank
end

to reset-view
  ask patches [set pcolor white set plabel "" set plabel-color black view-bc]
end

to view-bc
  ifelse left-bc = "no-flow"
  [ask patches with [pxcor = 0][set pcolor black]]
  [ask patches with [pxcor = 0][set pcolor blue]]

  ifelse right-bc = "no-flow"
  [ask patches with [pxcor = max-pxcor][set pcolor black]]
  [ask patches with [pxcor = max-pxcor][set pcolor blue]]

  ifelse top-bc = "no-flow"
  [ask patches with [pycor = max-pycor][set pcolor black]]
  [ask patches with [pycor = max-pycor][set pcolor blue]]

  ifelse bottom-bc = "no-flow"
  [ask patches with [pycor = min-pycor][set pcolor black]]
  [ask patches with [pycor = min-pycor][set pcolor blue]]

  ask patches with [fixed-head? = true][set pcolor blue]
  ask patches with [ET? = true][set pcolor brown]
  ask patches with [DRAIN? = true][set pcolor magenta]
  ask patches with [RIV? = true][set pcolor cyan]
end

to setup-maxmin-heads                                                    ;; finds max and min H values for colour-coding
  set max-head [H] of max-one-of patches with [interior-node? = true and no-flow? = false][H]
  set min-head [H] of min-one-of patches with [interior-node? = true and no-flow? = false][H]
end

to setup-maxmin-K                                                        ;; finds max and min K values for colour-coding
  set max-K [K-patch] of max-one-of patches [K-patch]
  set min-K [K-patch] of min-one-of patches [K-patch]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OTHER SIMULATION PROCEDURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GENERATE NAMES AND WRITE FILES FOR BEHAVIORSPACE EXPERIMENTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to output-results
  set max-monitoring-capacity_name ""
  set fine-magnitude_name ""
  set S-enforcement-cost_name ""
  set S-reputation_name ""
  set voluntary-compliance-level_name ""
  set rule-breaker-level_name ""
  set metanorm_name ""

  set max-monitoring-capacity_name (word "M=" max-monitoring-capacity "_")
  set fine-magnitude_name (word "F=" fine-magnitude "_")
  set S-enforcement-cost_name (word "Se=" S-enforcement-cost "_")
  set S-reputation_name (word "Sr=" S-reputation "_")
  set voluntary-compliance-level_name (word "Vc=" voluntary-compliance-level "_")
  set rule-breaker-level_name (word "Rb=" rule-breaker-level "_")
  set metanorm_name (word "Mn=" metanorm? "_")

  set filename (word max-monitoring-capacity_name fine-magnitude_name S-enforcement-cost_name S-reputation_name voluntary-compliance-level_name rule-breaker-level_name metanorm_name behaviorspace-run-number)

  ;export-all-plots (word filename "_plots.txt")
  ;export-interface (word filename ".png")
  ;export-view (word filename "_world.png")

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SAVE RESULTS TO TS LISTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to INITIALISE-TS-LISTS
  set TS-norm-strength []
  set TS-compliance []
  set TS-gini-wealth []
  set TS-decision-representative-farmer []
  set TS-drawdowns-mean []
  set TS-drawdowns-std []
  set TS-profits-mean []
  set TS-cummulative-wealth-median []
  set TS-total-breaches []
  set TS-boldness []
  set TS-vengefulness []
end

to SAVE-METRICS-TO-TS-LISTS
  set TS-norm-strength lput norm-strength TS-norm-strength
  set TS-compliance lput compliance TS-compliance
  set TS-drawdowns-mean lput drawdowns-mean TS-drawdowns-mean
  set TS-drawdowns-std lput drawdowns-std TS-drawdowns-std
  set TS-gini-wealth lput gini-index TS-gini-wealth
  set TS-profits-mean lput profits-mean TS-profits-mean
  set TS-decision-representative-farmer lput representative-farmer-choice TS-decision-representative-farmer
  set TS-cummulative-wealth-median lput cummulative-wealth-median TS-cummulative-wealth-median
  set TS-total-breaches lput total-breaches TS-total-breaches
  set TS-boldness lput mean-boldness TS-boldness
  set TS-vengefulness lput mean-vengefulness TS-vengefulness
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REPORTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report norm-strength
  report mean [vengefulness] of farmers - mean [boldness] of farmers
end

to-report compliance
  report (count farmers with [decision = "cooperate"] / num-farmers)
end

to-report drawdowns-mean
  report mean [Hground - H] of farmers
end

to-report drawdowns-std
  report standard-deviation [Hground - H] of farmers
end

to-report gini-index
  report (gini-index-reserve / num-farmers) / 0.5
end

to-report profits-mean
  report mean [E-score] of farmers
end

to-report profits-median
  report median [E-score] of farmers
end

to-report profits-std
  report standard-deviation [E-score] of farmers
end

to-report cummulative-wealth-median
  report median [cummulative-wealth] of farmers
end

to-report cummulative-wealth-std
  report standard-deviation [cummulative-wealth] of farmers
end

to-report representative-farmer-choice
  ask farmer 1 [ifelse decision = "defect" [set decision-of-representative-farmer 1][set decision-of-representative-farmer 0]]
  report decision-of-representative-farmer
end

to-report total-breaches
  report sum [breach] of farmers
end

to-report mean-boldness
  report mean [boldness] of farmers
end

to-report mean-vengefulness
  report mean [vengefulness] of farmers
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MEAN REPORTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report norm-strength-out
  report mean sublist (reverse TS-norm-strength) 0 5
end

to-report compliance-out
  report mean sublist (reverse TS-compliance) 0 5
end

to-report gini-wealth-out
  report mean sublist (reverse TS-gini-wealth) 0 5
end

to-report decision-representative-farmer-out
  report mean sublist (reverse TS-decision-representative-farmer) 0 5
end

to-report drawdowns-mean-out
  report mean sublist (reverse TS-drawdowns-mean) 0 5
end

to-report drawdowns-std-out
  report mean sublist (reverse TS-drawdowns-std) 0 5
end

to-report profits-mean-out
  report mean sublist (reverse TS-profits-mean) 0 5
end

to-report cummulative-wealth-median-out
  report mean sublist (reverse TS-cummulative-wealth-median) 0 5
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETUP EXPERIMENTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to SETUP-EXPERIMENT
  setup-GWmodel
  ask patches with [pxcor = max-pxcor]
  [set right-bc-head (left-bc-head + (regional-gradient / 100) * (delta * N))]
  initialize-heads
  SETUP-RANDOM
  ask farmers [set Hground H + 10]
  reset-ticks
  set year 0
end
@#$#@#$#@
GRAPHICS-WINDOW
632
66
1413
868
-1
-1
20.84
1
10
1
1
1
0
0
0
1
0
36
0
36
0
0
1
ticks
30.0

BUTTON
1421
114
1606
153
1: SETUP GWM
setup-GWmodel
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1712
1012
1869
1045
K
K
1
100
10
1
1
[m/day]
HORIZONTAL

SLIDER
1948
858
2076
891
delta-t
delta-t
1
365
182.5
0.1
1
[days]
HORIZONTAL

TEXTBOX
1712
995
1936
1013
HYDRAULIC PARAMETERS
12
0.0
1

INPUTBOX
2123
718
2197
778
N
35
1
0
Number

INPUTBOX
2213
718
2285
778
M
35
1
0
Number

INPUTBOX
2321
761
2386
821
delta
200
1
0
Number

CHOOSER
1712
722
1804
767
left-bc
left-bc
"no-flow" "fixed-head"
1

CHOOSER
1988
722
2080
767
right-bc
right-bc
"no-flow" "fixed-head"
1

CHOOSER
1898
722
1990
767
top-bc
top-bc
"no-flow" "fixed-head"
0

CHOOSER
1803
722
1899
767
bottom-bc
bottom-bc
"no-flow" "fixed-head"
0

INPUTBOX
1898
766
1990
826
top-bc-head
80
1
0
Number

INPUTBOX
1712
766
1804
826
left-bc-head
500
1
0
Number

INPUTBOX
1988
766
2080
826
right-bc-head
568.6
1
0
Number

INPUTBOX
1803
766
1900
826
bottom-bc-head
80
1
0
Number

MONITOR
2123
778
2197
823
Size X [km]
(delta * N) / 1000
17
1
11

MONITOR
2213
778
2285
823
Size Y [km]
(delta * M) / 1000
17
1
11

CHOOSER
2406
845
2672
890
view
view
"none" "wells" "K (values)" "K (contours)" "T (values)" "S (values)" "heads (values)" "heads (contours)" "areal recharge" "ET" "DRAIN conductance" "DRAIN flow [m3/d] and head values" "DRAIN flow [L/s] and head values" "RIV flow [m3/d] and head values" "RIV flow [L/s] and head values" "RIV flow [L/s] and drawdowns" "RIV storage and head values" "RIV bottom and head values" "RIV conductance" "patch numbering" "boundary conditions"
6

BUTTON
2539
889
2648
922
NIL
reset-view
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
1822
853
1934
898
solver
solver
"steady-state" "transient"
1

INPUTBOX
1712
913
1800
973
initial-heads
10
1
0
Number

SLIDER
1712
1045
1869
1078
aquifer-thickness
aquifer-thickness
1
100
50
1
1
[m]
HORIZONTAL

CHOOSER
1712
853
1824
898
aquifer-type
aquifer-type
"confined" "unconfined"
0

BUTTON
1421
293
1607
331
6: RUN
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

INPUTBOX
1865
1012
1941
1077
S
1.0E-4
1
0
Number

BUTTON
1923
1232
2077
1265
place pumping well
place-pumping-well
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1923
1265
2077
1298
place injection well
place-injection-well
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
855
10
1194
36
FlowLogo - Groundwater Commons Game
16
0.0
1

TEXTBOX
1703
1213
2066
1231
DISCHARGE AND RECHARGE WELLS
12
0.0
1

TEXTBOX
2251
1238
2433
1294
(input well discharge or injection rate, then press button and click on model window to add well or injection point)
11
0.0
1

TEXTBOX
2137
683
2287
701
MODEL DIMENSIONS
12
0.0
1

TEXTBOX
2157
701
2253
720
Number of cells
11
0.0
1

TEXTBOX
2522
828
2558
846
VIEWS
12
0.0
1

INPUTBOX
1942
1012
2018
1077
Sy
0.3
1
0
Number

BUTTON
1422
187
1605
226
3: INITIAL CONDITIONS GWM
initialize-heads
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
2203
751
2218
769
X
11
0.0
1

TEXTBOX
2311
741
2403
760
Size of cells [m]
11
0.0
1

BUTTON
2075
1232
2160
1265
hide patches
hide-stresses-patches
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
1701
1232
1774
1298
Qwell-input
20000
1
0
Number

INPUTBOX
1773
1232
1846
1298
Qinjection-input
500
1
0
Number

BUTTON
2075
1265
2160
1298
hide labels
hide-stresses-labels
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
2161
1232
2246
1298
clear all wells
clear-stresses
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1875
1078
1936
1096
confined S
11
0.0
1

TEXTBOX
1943
1079
2026
1098
unconfined S
11
0.0
1

TEXTBOX
1698
1351
1806
1381
AREAL RECHARGE
12
0.0
1

INPUTBOX
1697
1366
1786
1432
areal-recharge
0.2
1
0
Number

TEXTBOX
1698
1436
1932
1454
Recharge units: [m3/m2*day] = [m/day]
11
0.0
1

TEXTBOX
1703
1302
1894
1330
Pumping/Injection units: [m3/day]\nScreen level: [m] above reference
11
0.0
1

INPUTBOX
1843
1232
1922
1298
screen-level-input
20
1
0
Number

TEXTBOX
2030
1106
2355
1137
BOUNDARY CONDITIONS
12
0.0
1

BUTTON
2160
1126
2375
1159
change this cell to: fixed head
change-to-fixed-head
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
2030
1126
2166
1192
fixed-head-value-new
100
1
0
Number

BUTTON
2160
1161
2375
1194
change this cell to: no-flow
change-to-no-flow
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1427
599
1597
634
export heads to file
write-output-heads
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
1863
1125
1997
1158
set K for this cell
set-K
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1711
1125
1866
1185
K-input
10
1
0
Number

TEXTBOX
1713
1105
1999
1135
HYDRAULIC CONDUCTIVITY
12
0.0
1

BUTTON
1427
561
1596
601
NIL
reset-ticks
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
2015
1012
2268
1077
set these hydraulic parameters for all cells
setup-hydraulic-parameters
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
2159
1382
2359
1415
sine-recharge-multiplier?
sine-recharge-multiplier?
1
1
-1000

BUTTON
1985
1366
2135
1432
clear all recharge values
clear-recharge
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
1785
1366
1985
1399
set recharge for all patches
set-recharge-all-patches
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
1785
1401
1985
1434
set this value for a single patch
set-recharge-single-patch
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
2455
1282
2761
1366
This multiplier is a sine wave of recharge centered around the month of June. Rain is zero during the summer, end of spring and the begining of autumn. This multiplier is applied to all the patches with recharge. Settings can be modified in the code
11
0.0
1

SWITCH
2462
1242
2646
1275
sine-well-multiplier?
sine-well-multiplier?
1
1
-1000

TEXTBOX
1703
1666
1954
1684
EVAPOTRANSPIRATION
12
0.0
1

SLIDER
1913
1683
2085
1716
ET-extinction-depth
ET-extinction-depth
0
10
5
1
1
NIL
HORIZONTAL

INPUTBOX
1703
1683
1833
1743
ET-land-surface-elevation
50
1
0
Number

BUTTON
2083
1683
2189
1716
add ET cell
set-ET-patch
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
2187
1683
2291
1716
clear all ET cells
clear-ET-patches-all
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
1831
1683
1913
1743
ET-max-rate
0.001
1
0
Number

TEXTBOX
1707
1743
1857
1761
ET units = [m/day]
11
0.0
1

BUTTON
1428
700
1598
735
import K grid file
;import-SGS-grid
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
1703
1464
1878
1494
DRAIN/SPRING
12
0.0
1

INPUTBOX
1698
1481
1793
1541
DRAIN-elevation
40
1
0
Number

INPUTBOX
1795
1481
1908
1541
DRAIN-conductance
500
1
0
Number

TEXTBOX
1701
1541
1893
1559
Drain conductance units: [m2/day]
11
0.0
1

BUTTON
1907
1481
2021
1514
add drain cell
set-drain-patch
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
1857
1586
1955
1646
RIV-conductance
50
1
0
Number

INPUTBOX
1701
1586
1782
1646
RIV-elevation
50
1
0
Number

INPUTBOX
1781
1586
1860
1646
RIV-bottom
47
1
0
Number

TEXTBOX
1701
1569
1851
1587
LOSING/GAINING RIVER CONDITION
12
0.0
1

BUTTON
1955
1586
2057
1619
add RIV cell
set-RIV-patch
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
2021
1481
2144
1514
clear all DRAIN cells
clear-DRAIN-patches-all
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
2055
1586
2158
1619
clear all RIV cells
clear-RIV-patches-all
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
2429
889
2539
922
NIL
refresh-view
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
1428
634
1598
669
export world
export-world \"exampleGWABM\"
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
1428
666
1598
701
import world
import-world \"exampleGWABM\"
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
1428
734
1598
769
NIL
set-initial-heads
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
858
43
915
88
year
YEAR
17
1
11

INPUTBOX
2413
1120
2498
1198
Qfixed-flux
2000
1
0
Number

BUTTON
2500
1140
2650
1175
place fixed flux cell
place-fixed-flux
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
2416
1103
2604
1126
FIXED-FLUX
12
0.0
1

BUTTON
1422
223
1605
262
4: SETUP SOCIAL/RANDOM
SETUP-RANDOM
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1997
47
2171
80
num-farmers
num-farmers
10
1500
10
1
1
NIL
HORIZONTAL

SLIDER
1704
303
1918
336
pumping-cap
pumping-cap
0
1
0.2
0.1
1
NIL
HORIZONTAL

SLIDER
2210
462
2384
495
mutation-probability
mutation-probability
0
1
0.05
0.01
1
NIL
HORIZONTAL

SWITCH
2209
508
2325
541
metanorm?
metanorm?
1
1
-1000

SWITCH
2208
551
2322
584
memory?
memory?
0
1
-1000

SWITCH
2205
597
2339
630
REPUTATION?
REPUTATION?
0
1
-1000

SLIDER
2324
551
2491
584
memory-length
memory-length
1
10
2
1
1
years
HORIZONTAL

SLIDER
2524
597
2699
630
memory-reputation
memory-reputation
0
100
21
1
1
NIL
HORIZONTAL

SLIDER
2345
596
2518
629
S-bad-reputation
S-bad-reputation
0
100
100
1
1
NIL
HORIZONTAL

SLIDER
1707
147
1940
180
IWA
IWA
0
15
5.6
0.1
1
[ML/ha]
HORIZONTAL

CHOOSER
1705
337
1918
382
monitoring-style
monitoring-style
"flat" "adaptive"
0

SLIDER
2203
376
2395
409
risk-regulation-factor
risk-regulation-factor
1
20
1
1
1
NIL
HORIZONTAL

TEXTBOX
1704
275
1863
319
INSTITUTION (I)
18
0.0
1

SLIDER
1919
381
2082
414
fine-magnitude
fine-magnitude
0
1
0
0.05
1
NIL
HORIZONTAL

TEXTBOX
1705
12
1883
36
ECONOMICS (E)
18
0.0
1

TEXTBOX
1709
446
1827
490
SOCIAL (S)
18
0.0
1

SLIDER
1703
473
1929
506
S-enforcement-cost
S-enforcement-cost
-1
1
0.67
0.05
1
NIL
HORIZONTAL

SLIDER
1703
506
1929
539
S-reputation
S-reputation
0
1
0.58
0.05
1
NIL
HORIZONTAL

SLIDER
1707
180
1940
213
crop-yield
crop-yield
0
10
30
0.1
1
[ton/ha]
HORIZONTAL

SLIDER
1707
113
1940
146
crop-price
crop-price
0
600
1300
1
1
[MAD/ton]
HORIZONTAL

MONITOR
2196
111
2328
156
returns [MAD/ha]
crop-yield * crop-price
1
1
11

SLIDER
1957
147
2184
180
variable-costs-other
variable-costs-other
0
3000
19116
1
1
[MAD/ha]
HORIZONTAL

SLIDER
1957
180
2183
213
pump-efficiency
pump-efficiency
0
1
0.84
0.1
1
NIL
HORIZONTAL

SLIDER
1957
113
2183
146
energy-price
energy-price
0
1
1.254
0.01
1
[MAD/kWh]
HORIZONTAL

TEXTBOX
2022
96
2189
116
Variable costs
14
0.0
1

SLIDER
1707
211
1941
244
growing-season
growing-season
0
365
181
1
1
[days]
HORIZONTAL

SLIDER
1957
213
2184
246
water-efficiency
water-efficiency
0
1
1
0.1
1
NIL
HORIZONTAL

MONITOR
1017
43
1095
88
compliance
compliance-level
2
1
11

PLOT
17
258
312
378
compliance rate %
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if model-ready? = true [plot compliance]"

SWITCH
856
904
978
937
economics
economics
0
1
-1000

SWITCH
977
904
1096
937
institution
institution
0
1
-1000

SWITCH
1096
904
1200
937
social
social
0
1
-1000

PLOT
17
13
312
133
boldness and vengefulness
time
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"boldness" 1.0 0 -2674135 true "" "if model-ready? = true [plot mean [boldness] of farmers]"
"vengefulness" 1.0 0 -14070903 true "" "if model-ready? = true [plot mean [vengefulness] of farmers]"

PLOT
19
1109
438
1269
histrogram of farmer happiness (total score)
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "if model-ready? = true [set-plot-x-range 0 max [score] of farmers + 1]\nif model-ready? = true [set-plot-y-range 0 num-farmers / 2]"
PENS
"default" 10.0 1 -16777216 true "" "if model-ready? = true [histogram [score] of farmers]"

MONITOR
445
1111
608
1156
NIL
mean [score] of farmers
0
1
11

PLOT
314
258
596
378
histogram — boldness
NIL
NIL
0.0
1.0
0.0
30.0
true
false
"" "set-histogram-num-bars 20\nset-plot-x-range 0 1.1"
PENS
"default" 0.05 1 -2674135 true "" "if model-ready? = true [histogram [boldness] of farmers]"

PLOT
315
380
595
500
histogram — vengefulness
NIL
NIL
0.0
1.0
0.0
10.0
true
false
"" "set-histogram-num-bars 20\nset-plot-x-range 0 1"
PENS
"default" 0.05 1 -14070903 true "" "if model-ready? = true [histogram [vengefulness] of farmers]"

PLOT
315
503
595
626
Lorenz curve — Wealth
Pop %
Wealth %
0.0
100.0
0.0
100.0
false
true
"" ""
PENS
"wealth" 1.0 0 -2674135 true "" "if model-ready? = true\n[\nplot-pen-reset\nset-plot-pen-interval 100 / num-farmers\nplot 0\nforeach lorenz-points plot\n]"
"equal" 100.0 0 -16777216 true "plot 0\nplot 100" ""

PLOT
17
503
312
625
Gini Index v. Time
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"wealth" 1.0 0 -2674135 true "" "if model-ready? = true [plot (gini-index-reserve / num-farmers) / 0.5]"

PLOT
19
635
598
755
decision sequence of a representative farmer
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "if model-ready? = true [ask farmer 1 [ifelse decision = \"defect\" [plot 1][plot 0]]]"

TEXTBOX
1797
90
1859
113
Income
14
0.0
1

PLOT
20
762
597
941
average groundwater drawdowns
NIL
[m] below surface
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"pen-1" 1.0 0 -7500403 true "" "if model-ready? = true [plot mean [Hground - H] of farmers with [pxcor >= min-pxcor and pxcor <= max-pxcor and pycor >= min-pycor and pycor <= max-pycor]]"

SLIDER
1705
381
1920
414
max-monitoring-capacity
max-monitoring-capacity
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
1704
554
1927
587
voluntary-compliance-level
voluntary-compliance-level
0
1
0
0.01
1
NIL
HORIZONTAL

BUTTON
1937
553
2012
587
update
update-voluntary-compliance
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
17
380
312
500
total volume of breaches [ML]
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
"default" 1.0 0 -16777216 true "" "plot sum [breach] of farmers"

MONITOR
1094
43
1144
88
M
monitoring-percent
17
1
11

PLOT
314
13
596
256
strength of social norms
mean boldness
mean vengefulness
0.0
1.0
0.0
1.0
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if model-ready? = true\n[\nif mean [boldness] of farmers < 0.3 and mean [vengefulness] of farmers > 0.7 [set-plot-pen-color blue]\nif mean [boldness] of farmers > 0.7 and mean [vengefulness] of farmers < 0.3 [set-plot-pen-color red]\nif (mean [boldness] of farmers <= 0.7 and mean [boldness] of farmers >= 0.3) or (mean [vengefulness] of farmers <= 0.7 and mean [vengefulness] of farmers >= 0.3) [set-plot-pen-color black]\nplotxy mean [boldness] of farmers mean [vengefulness] of farmers\n]"

CHOOSER
1919
337
2081
382
enforcement-strategy
enforcement-strategy
"random" "risk-based"
0

SWITCH
2203
339
2393
372
graduated-sanctions?
graduated-sanctions?
1
1
-1000

PLOT
17
135
312
255
norm strength
NIL
NIL
0.0
10.0
-1.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if model-ready? = true [plot mean [vengefulness] of farmers - mean [boldness] of farmers]"

SLIDER
1705
613
1928
646
rule-breaker-level
rule-breaker-level
0
1
0
0.01
1
NIL
HORIZONTAL

BUTTON
1937
613
2012
647
update
update-rule-breakers
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
2196
155
2342
200
variable costs [MAD/ha]
mean [VC] of farmers
0
1
11

MONITOR
2196
201
2335
246
mean profits [MAD/ha]
mean [IN + VC] of farmers
0
1
11

PLOT
19
955
602
1098
mean economic performance of farms [$/ha]
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"mean variable costs" 1.0 0 -1184463 true "" "if model-ready? = true [plot mean [VC] of farmers]"
"mean profits" 1.0 0 -13345367 true "" "if model-ready? = true [plot mean [IN + VC] of farmers]"
"mean returns" 1.0 0 -7500403 true "" "if model-ready? = true [plot mean [IN] of farmers]"
"break-even point" 1.0 0 -2674135 true "" "if model-ready? = true [plot 0]"

MONITOR
2198
68
2327
113
mean farm area [ha]
mean [farm-area-total] of farmers
0
1
11

SLIDER
2123
891
2331
924
inflow-to-basin
inflow-to-basin
0
2000
200
1
1
[m3/day]
HORIZONTAL

MONITOR
2424
713
2524
758
total recharge
sum [Qinjection] of patches with [injection? = true]
0
1
11

MONITOR
2527
713
2625
758
total outflows
sum [Qwell] of patches with [interior-node? = true and injection? = false]
0
1
11

SWITCH
1427
528
1596
561
social-model?
social-model?
0
1
-1000

BUTTON
1422
258
1607
294
5: SET GROUND SURFACE
ask farmers [set Hground H + 10]
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
1425
495
1598
529
SETUP MODEL
SETUP-EXPERIMENT
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
2434
774
2610
807
buffer
buffer
0
10
3
1
1
NIL
HORIZONTAL

SLIDER
2123
848
2331
881
regional-gradient
regional-gradient
0
1
0.98
0.01
1
%
HORIZONTAL

BUTTON
1421
153
1606
189
2: SETUP REGIONAL FLOW
ask patches with [pxcor = max-pxcor]\n[set right-bc-head (left-bc-head + (regional-gradient / 100) * (delta * N))]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
1705
42
1983
87
economy?
economy?
"Australia: Cotton(S), Vetch(W)" "Central Valley: Almonds(S)" "Punjab: Rice(S), Wheat(W)" "North China Plain: Corn(S), Wheat(W)" "Morocco-Tadla: Onion(S), Sugarbeet(W)"
4

MONITOR
196
904
311
949
drawdown mean
mean [Hground - H] of farmers with [pxcor >= min-pxcor and pxcor <= max-pxcor and pycor >= min-pycor and pycor <= max-pycor]
1
1
11

MONITOR
311
904
411
949
drawdown std
standard-deviation [Hground - H] of farmers with [pxcor >= min-pxcor and pxcor <= max-pxcor and pycor >= min-pycor and pycor <= max-pycor]
1
1
11

MONITOR
916
43
1021
88
growing season
growing-season?
1
1
11

PLOT
19
1276
434
1426
histogram of farmer profits (E-score)
NIL
NIL
-1000.0
10000.0
0.0
20.0
false
false
"" "if model-ready? = true [set-plot-x-range 0 max [E-score] of farmers + 1]\nif model-ready? = true [set-plot-y-range 0 num-farmers / 2]"
PENS
"default" 10.0 1 -16777216 true "" "if model-ready? = true [histogram [E-score] of farmers]"

MONITOR
441
1278
607
1323
NIL
mean [E-score] of farmers
0
1
11

MONITOR
447
1161
609
1206
NIL
median [score] of farmers
0
1
11

MONITOR
441
1328
609
1373
NIL
median [E-score] of farmers
0
1
11

TEXTBOX
1937
482
2106
501
GRID=1-S-enforcement-cost
11
0.0
1

TEXTBOX
1708
589
1935
607
% compliance advocates (social capital)
11
0.0
1

TEXTBOX
1938
518
2088
536
GROUP=S-reputation
11
0.0
1

TEXTBOX
1712
649
1862
667
% rule breakers
11
0.0
1

MONITOR
1143
43
1200
88
F
fine-magnitude
17
1
11

TEXTBOX
1715
694
1951
738
GROUNDWATER MODEL
18
0.0
1

TEXTBOX
2328
498
2478
516
NIL
11
0.0
1

TEXTBOX
849
942
1234
970
components of the agent objective function can be turned on/off here
11
0.0
1

TEXTBOX
912
882
930
902
E
16
0.0
1

TEXTBOX
1032
882
1048
902
I
16
0.0
1

TEXTBOX
1139
882
1154
902
S
16
0.0
1

TEXTBOX
1425
65
1610
121
after setting model parameters, follow the sequence below to initialise and run the model:
11
0.0
1

TEXTBOX
2391
464
2541
492
probability that the agent will try a new strategy
11
0.0
1

BUTTON
759
1011
901
1046
setup and burn in
set managent-period-started? False\n\n;;STORE PARAMETERS FOR THIS SIMULATION\nlet old-pumping-cap pumping-cap\nlet old-max-monitoring-capacity max-monitoring-capacity\nlet old-fine-magnitude fine-magnitude\nlet old-S-enforcement-cost S-enforcement-cost\nlet old-S-reputation S-reputation\nlet old-voluntary-compliance-level voluntary-compliance-level\nlet old-rule-breaker-level rule-breaker-level\nlet old-metanorm? metanorm?\nlet old-monitoring-style monitoring-style\nlet old-enforcement-strategy enforcement-strategy\nlet old-graduated-sanctions? graduated-sanctions?\n\n;;SETS UP THE GROUNDWATER AND SOCIAL COMPONENTS\nset num-farmers 150\nSETUP-EXPERIMENT\n\n;;SETS PARAMETERS FOR THE BURN-IN PERIOD\nset pumping-cap 0.2\nset max-monitoring-capacity 0.1\nset fine-magnitude 0.1\nset voluntary-compliance-level 0\nset rule-breaker-level 0\nset metanorm? false\nset monitoring-style \"flat\"\nset enforcement-strategy \"random\"\nset graduated-sanctions? false\n\n;;SET ECONOMIC PARAMETERS\nset economy? \"Morocco-Tadla: Corn(S), Wheat(W)\"\n\n;;RUN THE BURN-IN PERIOD\nrepeat 50 [go]\nset managent-period-started? True\nreset-ticks\nset year 0\n\n;;RELOAD EXPERIMENT PARAMETERS SAVED AT THE START\nset pumping-cap old-pumping-cap\nset max-monitoring-capacity old-max-monitoring-capacity\nset fine-magnitude old-fine-magnitude\nset S-enforcement-cost old-S-enforcement-cost\nset S-reputation old-S-reputation\nset voluntary-compliance-level old-voluntary-compliance-level\nset rule-breaker-level old-rule-breaker-level\nset metanorm? old-metanorm?\nset monitoring-style old-monitoring-style\nset enforcement-strategy old-enforcement-strategy\nset graduated-sanctions? old-graduated-sanctions?\nupdate-voluntary-compliance\nupdate-rule-breakers
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

Aquifers are a climate-resilient source of water in the world’s main agricultural regions. These resources, however, have undergone rapid depletion in recent decades (Aeschbach-Hertig & Gleeson 2012; Richey et al. 2015; Gleeson 2012).

Rules governing human behaviour are at the heart of any system of natural resource management. In the context of groundwater conservation, rules usually take the form of pumping restrictions, which will be received differently depending on the beliefs and cultural values of each specific community. Pumping restrictions are also difficult to monitor and enforce given the spread of groundwater use across vast geographical areas. Violators are constantly tempted to gain an economic advantage over complying farmers, from higher yields and/or increased irrigated acreage derived from any water taken (illegally) above the allocated limits (this may or may not be detected by the regulator). These breaches, however, increase the economic costs of everyone (including violators) due to larger pumping lifts, with no single user bearing the full cost of a breach. This situation leads farmers toward a ‘tragedy of the commons’ (Ostrom 1990). Comprehensive enforcement is therefore rarely attainable or possible. Thus, without compliance, rules are meaningless so understanding the mechanims that may lead to effective compliance is essential if conservation is to be successful.

Managers typically evaluate the performance of enforcement and compliance policies via field surveys and interviews. These instruments however may not reveal the true motivations and attitudes towards restricted water allocations. The cost and time of conducting empirical research can be significant. These are major challenges for decision-makers searching for courses of action that lead to long-term compliance and extend economic activity in groundwater basins subject to depletion. Due to the difficulty of studying rule-breaking behaviour directly, new modelling approaches need to be explored.

We have designed the 'Groundwater Commons Game' (GCG) to investigate mechanisms that may lead to the emergence and persistence of groundwater conservation, taking into consideration the varying cultural values and beliefs of human societies, as revealed by the World Values Survey. The model aims to complement the efforts of empirical research, presenting a novel exploratory approach to shed light on potential pathways to global groundwater conservation.

The GCG builds on Robert Axelrod’s seminal paper on the emergence of social norms (Axelrod, 1986). Here, we adapt Axelrod’s framework to study social norms of compliance in the context of groundwater conservation. The GCG is designed to be very general and the approach can be applied to any number of problems involving stressed groundwater systems. The structure and assumptions of the GCG can be easily tailored and/or extended to suit specific economic, institutional, social, and hydrogeological contexts. The game is implemented using the FlowLogo platform (Castilla-Rho et al., 2015).

In the GCG, agents represent farmers in a hypothetical groundwater basin where a primary crop is grown, e.g., Cotton. The problem is intentionally designed to be water-limited, focusing on the issue of managing compliance through drought periods. Rainfall is not sufficient to irrigate crops and the underlying aquifer is used to supply the water required to maximise crop yields. The model can be extended to include a more detailed description of the farm enterprise (e.g., crop rotations, deficit irrigation, etc). Here however, we develop a simple model with flexible and customizable input data, whic can be used as a basis for applications in specific farming contexts.

The model represents a hypothetical basin managed under a system of non-transferable entitlements or water rights. Because our focus is exclusively on the issue of social norms, we do not consider trading of entitlements, as this would incorporate an additional and unnecessary level of complexity to our analysis. Trading rules however, could be easily implemented in our model.

In our hypothetical basin, drought and overexploitation have led to restrictions on groundwater withdrawals. Each cropping season, the regulator imposes a cap on groundwater entitlements. The cap applies equally to all agents. Capped groundwater withdrawals constrain farmers’ profits, which have to cut back from the ideal levels of irrigation that maximize crop yields and/or the irrigated acreage.  Farmers in the GCG are ‘selfish’ agents in terms of behaving opportunistically whenever they have the chance to pump more water than an allocated limit. Strategically, they are self-interested and modify their behaviour only to maximize their ‘fitness’ (details on how fitness is calculated are provided below).

Our motivation is to explore how social norms sustaining high levels of compliance with water allocations can emerge in this context. We are interested in factors thay may trigger norms from the bottom-up.

Some research questions:

●	What mechanisms may trigger social norms that sustain compliance to restricted groundwater allocations in agricultural communities?
●	Can any of these mechanisms significantly alter the system’s behaviour with a small amount of management effort/resources?
●	Are rigorous monitoring and enforcement the only way to deter breaches and achieve compliance?
●	What role do social norms and cultural values
play in the emergence or collapse of compliance norms ?
●	In which countries is groundwater conservation more likely to succeed?
●	What general policy recommendations can we make?

Key aims:

●	Present a model that is general and applicable to aquifers everywhere. The framework is simple, but can be parameterized with empirical data if available/needed.
●	Drive agent behaviours with cultural variables derived from the World Values Survey
●	Demonstrate the application of the model in three real-world scenarios the Punjab (India/Pakistan), the Central Valley (USA), and the Murray-Darling Basin
(Australia)
●	Compare simulated patterns of compliance with observations in the case of the Murray-Darling Basin (where the authors have conducted extensive social research on enforcement and compliance),
●	Use the GCG as a basis to propose compliance and enforcement policy recommendations that apply broadly.

![Example](file:Figure1.png)

## HOW IT WORKS

In the GCG, an agent’s strategy has two dimensions: the propensity to defect (boldness, B), and the propensity to punish (vengefulness, V). B and V are continuous variables between 0 and 1. For instance, an agent with B=0.8 and V=0.8 is very likely to defect, but also very likely to punish other farmers breaching the seasonal allocation (a very smart, yet dishonest farmer). On the other hand, an agent with B=0 and V=1 could be considered as a strong rule-follower. Many combinations are possible. The population averages of B and V define the presence or absence of a norm against defection. Following Axelrod’s definition: a social norm of compliance emerges when B~0 and V~1 become a stable and long-term condition among agents.

The emergence of a norm is modelled by allowing farmers modifying their strategies (B,V) based on the evolutionary principles of imitation and exploration. At the end of each growing season, agents look at their neighbours and copy (imitate) the most successful strategy of that year, using a fitness metric as a basis of comparison: we define this metric as the farmer’s performance index (Pindex). If an agent scores higher than its neighbours, he maintains the current strategy for the following year. With a given probability (mutation), agents change their boldness and vengefulness level to a random value, overriding the imitation mechanism. In other words, agents occasionally explore completely new strategies. As in Axelrod’s work, we do not impose any constraint or make any a priori assumption about the boldness (B) or vengefulness (V) of agents. Strategies are spontaneously being created, explored and abandoned, and information about each others' performance is shared by local social interactions (i.e., looking over the fence).

![Example](file:Figure2.png)

The GCG simulates the evolution of norms of compliance with allocations as evolutionary process. The strength of a norm is the difference between the population averages of B and V. If B is significantly higher than V, or if there is not much difference between them, we have a weak norm (most agents are pumping more water that they are supposed to, and not punishing breaches). On the other hand, when agents consistently select strategies having high V and low B, we can say that a norm of compliance has emerged. The GCG can be used to investigate the stability, growth and decay of these norms and the different conditions under which this happens.

During GCG runs, agents face the decision of whether to cooperate with the allocations (pump a fraction of their entitlement as required by the regulator) or to defect (pump more than the allocation). Each opportunity to defect comes with a probability of neighbouring farmers seeing that breach and reporting it to the regulator. This opportunity is represented by the probability of vengefulness P(V). P(V) is drawn from a random uniform distribution on the interval [0,1], at every turn for each agent. When V < P(V) an agent chooses to punish a defector. Similarly, there is a probability that an agent with defect, P(B). If B < P(B) the probability of defecting is higher than the agent’s boldness and therefore it decides to defect, otherwise it cooperates. As noted earlier, breaches impose higher economic costs to other agents, due to the widening/deepening of the cone of depression.

![Example](file:Figure3.png)

The above interactions result in a series of benefits and costs to each farmer, which are captured and quantified in the farmer’s performance index, Pindex=E*I*S. This index has three components: (i) an economic score (E) (ii) an institutional score (I), and (iii) a social score (S). The score is an indicator of a farmer’s success, everywhere and at every moment, taking into account these three dimensions of decision-making. The score of a farmer depends not only on his strategy (Bi, Vi), but also on the strategies of his neighbours (Bj, Vj).

![Example](file:Figure4.png)

The figure below illustrates the interaction between any two agents, showing the benefits (+) and costs (-) that apply in the neighbourhood of a breach.

![Example](file:Figure5.png)

Another way to think about the dynamics of the GCG is consider that each growing season farmers simultaneously play two games: _defect-or-not_ and _punish-or-not_. The former is driven by the farmers boldness B, the latter by their vengefulness V. The figure below represents the three components of the farmer score (economic, institutional, social) as ‘forces’ pulling a farmer’s decision in different directions. The objective of our model is to propose mechanisms that ‘pull’ the decisions of the majority of farmers to the right.

![Example](file:Figure6.png)

Each year, farmer agents play the GCG as described above. They calculate their score at the end of each growing season (assumed here as 180 days). Strategies for the following period are updated using the evolutionary approach. Simulations thus consider a large number of farmers following a diversity of strategies:

![Example](file:Figure7.png)

Decisions propagate through a complex network of local interactions, and indirectly via well interferences:

![Example](file:Figure8.png)

![Example](file:Figure9.png)

## ECONOMIC SCORE (E)

![Example](file:Figure_E.png)

## INSTITUTIONAL SCORE (I)

![Example](file:Figure_I.png)

## SOCIAL SCORE (S)

![Example](file:Figure_S.png)


## HOW TO USE IT

### 1: Set model parameters

![Example](file:Figure10.png)

### 2: Initialise the model

![Example](file:Figure11.png)

### 3: Run model

![Example](file:Figure13.png)

### 4: While the model is running, you can turn on/off social, economic or institutional mechanisms

![Example](file:Figure14.png)

### 5: You can explore the effects of changing institutional and social parameters as the simulation is running

![Example](file:Figure17.png)


## THE GROUNDWATER MODEL

The groundwater model is pre-loaded with hydrogeological parameters characteristic of regional flow conditions in alluvial settings. The groundwater sub model represents a 10x10 km basin, discretised into 40x40 cells. The dimension of each cell is 200 m. Model boundary conditions are defined by a no-flow boundary to the North and South, and constant head boundary cells to the East and West; setting head values to create an East-West gradient of 1/1000 representing typical conditions in regional aquifer systems. Underlying this basin is a semi-confined sand aquifer of 50 m model is transient with a time step of six months. We used a steady-state run with no pumping stresses as the initial condition for each simulation.

All hydrogeological parameters can be changed with frugal user intervention via the provided interface.

## BEHAVIOURSPACE EXPERIMENTS

We include BehaviourSpace experiments to explore the full parameter space of the model. Due to the long computation times needed for multiple realisations of each experiment, simulations were run in a high-performance computer cluster.

![Example](file:Figure16.png)

## THINGS TO NOTICE

In the left part of the interface, we provide a range of graphs summarizing system outputs

![Example](file:Figure15.png)


## FUTURE/CURRENT WORK

Current and future development of the GCG will be focused on:

●	Implementing a faster, iterative solver for the groundwater flow equations base don a preconditioned conjugate gradient (PCG) solver
●	Develop a faster version of FlowLogo based on the analytic element method

## NETLOGO FEATURES

This model was developed using FlowLogo—an agent-based groundwater flow simulator—which is documented in the following publication:

http://www.sciencedirect.com/science/article/pii/S136481521530044X

FlowLogo essentially allows the user to effortlessly link and agent-based model describing the socio-economic dynamics of a groundwater management problem with the physics of groundwater flow.

FlowLogo model files, documentation, MODFLOW checks, and ODD descriptions of example models can be found in the OpenABM repository:

https://www.openabm.org/model/4338/version/1/view

## RELATED MODELS

The GCG applies Robert Axelrod’s norms game (Axelrod, 1986) to the context of groundwater conservation. Key changes to Axelrod’s original model are:

1.	We consider local interactions between neighbouring agents, instead of a “soup” model as in Axelrod’s case, where all agents interact with each other. In our model, monitoring and punishment is local instead of global.
2.	We incorporate the physics of groundwater flow as a driver of agent behaviour. We explicitly quantify the economic damage (externality) imposed by defecting wells on other wells, which manifests as additional pumping costs incurred within a neighbourhood of the defecting well. The extent of this neighbourhood is determined by the local hydrogeological conditions and the magnitude of a breach.
3.	We use a combined score metric to quantify ‘fitness’ of an agent at every moment, based on economic, institutional and social factors. In the Axelrod case, fitness is entirely driven by the social component.
4.	We introduce heterogeneity in the magnitude of breaches, as we would expect in agricultural settings.
5.	Breaches are not only enforced by agents, but also by a higher authority: the water regulator. Enforcement therefore occurs at the local and basin scales.
6.	Besides implementing the main norm-enforcing mechanism proposed by Axelrod—metanorms—we explore additional mechanisms relevant for groundwater cases: law, voluntary compliance, internalisation, graduated sanctions, reputation, and memory.


## CREDITS AND REFERENCES

Aeschbach-Hertig, W. & Gleeson, T. Regional strategies for the accelerating global problem of groundwater depletion. Nature Geoscience 5, 853–861 (2012).

Axelrod, R. An evolutionary approach to norms. American Political Science Review 80, 1095–1111 (1986).

Castilla-Rho, J. C., Mariethoz, G., Rojas, R., Andersen, M. S. & Kelly, B. F. J. An agent-based platform for simulating complex human–aquifer interactions in managed groundwater systems. Environmental Modelling & Software 73, 305–323 (2015).

Gleeson, T., Wada, Y., Bierkens, M. F. P. & van Beek, L. P. H. Water balance of global aquifers revealed by groundwater footprint. Nature 488, 197–200 (2012).

Ostrom, E. Governing the Commons. (Cambridge University Press, 1990).

Richey, A. S. et al. Quantifying renewable groundwater stress with GRACE. Water Resour. Res. 51, 5217–5238 (2015).
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

house bungalow
false
0
Rectangle -7500403 true true 210 75 225 255
Rectangle -7500403 true true 90 135 210 255
Rectangle -16777216 true false 165 195 195 255
Line -16777216 false 210 135 210 255
Rectangle -16777216 true false 105 202 135 240
Polygon -7500403 true true 225 150 75 150 150 75
Line -16777216 false 75 150 225 150
Line -16777216 false 195 120 225 150
Polygon -16777216 false false 165 195 150 195 180 165 210 195
Rectangle -16777216 true false 135 105 165 135

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line bottom
false
0
Rectangle -7500403 true true 0 285 300 300

line half
true
0
Line -7500403 true 150 0 150 150

line left
false
0
Rectangle -7500403 true true 0 0 15 300

line right
false
0
Rectangle -7500403 true true 285 0 300 300

line right and below
false
0
Rectangle -7500403 true true 0 285 300 300
Rectangle -7500403 true true 285 0 300 285

line top
false
0
Rectangle -7500403 true true 0 0 300 15

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

person farmer
false
0
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Polygon -1 true false 60 195 90 210 114 154 120 195 180 195 187 157 210 210 240 195 195 90 165 90 150 105 150 150 135 90 105 90
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 79 172 94
Polygon -13345367 true false 120 90 120 180 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 180 90 172 89 165 135 135 135 127 90
Polygon -6459832 true false 116 4 113 21 71 33 71 40 109 48 117 34 144 27 180 26 188 36 224 23 222 14 178 16 167 0
Line -16777216 false 225 90 270 90
Line -16777216 false 225 15 225 90
Line -16777216 false 270 15 270 90
Line -16777216 false 247 15 247 90
Rectangle -6459832 true false 240 90 255 300

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
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="SC_MDB_50" repetitions="50" runMetricsEveryStep="false">
    <setup>;;STORE PARAMETERS FOR THIS SIMULATION
let old-pumping-cap pumping-cap
let old-max-monitoring-capacity max-monitoring-capacity
let old-fine-magnitude fine-magnitude
let old-S-enforcement-cost S-enforcement-cost
let old-S-reputation S-reputation
let old-voluntary-compliance-level voluntary-compliance-level
let old-rule-breaker-level rule-breaker-level
let old-metanorm? metanorm?
let old-monitoring-style monitoring-style
let old-enforcement-strategy enforcement-strategy
let old-graduated-sanctions? graduated-sanctions?

;;SETS UP THE GROUNDWATER AND SOCIAL COMPONENTS
set num-farmers 10
SETUP-EXPERIMENT

;;SETS PARAMETERS FOR THE BURN-IN PERIOD
set pumping-cap 0.2
set max-monitoring-capacity 0.1
set fine-magnitude 0.1
set voluntary-compliance-level 0
set rule-breaker-level 0
set metanorm? false
set monitoring-style "flat"
set enforcement-strategy "random"
set graduated-sanctions? false

;;SET ECONOMIC PARAMETERS
set economy? "Australia: Cotton(S), Vetch(W)"

;;RUN THE BURN-IN PERIOD
repeat 100 [go]
reset-ticks
set year 0

;;RELOAD EXPERIMENT PARAMETERS SAVED AT THE START
set pumping-cap old-pumping-cap
set max-monitoring-capacity old-max-monitoring-capacity
set fine-magnitude old-fine-magnitude
set S-enforcement-cost old-S-enforcement-cost
set S-reputation old-S-reputation
set voluntary-compliance-level old-voluntary-compliance-level
set rule-breaker-level old-rule-breaker-level
set metanorm? old-metanorm?
set monitoring-style old-monitoring-style
set enforcement-strategy old-enforcement-strategy
set graduated-sanctions? old-graduated-sanctions?
update-voluntary-compliance
update-rule-breakers</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>TS-norm-strength</metric>
    <metric>TS-compliance</metric>
    <metric>TS-gini-wealth</metric>
    <metric>TS-decision-representative-farmer</metric>
    <metric>TS-drawdowns-mean</metric>
    <metric>TS-drawdowns-std</metric>
    <metric>TS-profits-mean</metric>
    <metric>TS-cummulative-wealth-median</metric>
    <metric>TS-total-breaches</metric>
    <metric>TS-boldness</metric>
    <metric>TS-vengefulness</metric>
    <enumeratedValueSet variable="pumping-cap">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-monitoring-capacity">
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fine-magnitude">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-enforcement-cost">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-reputation">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="voluntary-compliance-level">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-breaker-level">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorm?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="monitoring-style">
      <value value="&quot;flat&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-strategy">
      <value value="&quot;random&quot;"/>
      <value value="&quot;risk-based&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graduated-sanctions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SC_CV_50" repetitions="50" runMetricsEveryStep="false">
    <setup>;;STORE PARAMETERS FOR THIS SIMULATION
let old-pumping-cap pumping-cap
let old-max-monitoring-capacity max-monitoring-capacity
let old-fine-magnitude fine-magnitude
let old-S-enforcement-cost S-enforcement-cost
let old-S-reputation S-reputation
let old-voluntary-compliance-level voluntary-compliance-level
let old-rule-breaker-level rule-breaker-level
let old-metanorm? metanorm?
let old-monitoring-style monitoring-style
let old-enforcement-strategy enforcement-strategy
let old-graduated-sanctions? graduated-sanctions?

;;SETS UP THE GROUNDWATER AND SOCIAL COMPONENTS
set num-farmers 50
SETUP-EXPERIMENT

;;SETS PARAMETERS FOR THE BURN-IN PERIOD
set pumping-cap 0.2
set max-monitoring-capacity 0.1
set fine-magnitude 0.1
set voluntary-compliance-level 0
set rule-breaker-level 0
set metanorm? false
set monitoring-style "flat"
set enforcement-strategy "random"
set graduated-sanctions? false

;;SET ECONOMIC PARAMETERS
set economy? "Central Valley: Almonds(S)"

;;RUN THE BURN-IN PERIOD
repeat 100 [go]
reset-ticks
set year 0

;;RELOAD EXPERIMENT PARAMETERS SAVED AT THE START
set pumping-cap old-pumping-cap
set max-monitoring-capacity old-max-monitoring-capacity
set fine-magnitude old-fine-magnitude
set S-enforcement-cost old-S-enforcement-cost
set S-reputation old-S-reputation
set voluntary-compliance-level old-voluntary-compliance-level
set rule-breaker-level old-rule-breaker-level
set metanorm? old-metanorm?
set monitoring-style old-monitoring-style
set enforcement-strategy old-enforcement-strategy
set graduated-sanctions? old-graduated-sanctions?
update-voluntary-compliance
update-rule-breakers</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>TS-norm-strength</metric>
    <metric>TS-compliance</metric>
    <metric>TS-gini-wealth</metric>
    <metric>TS-decision-representative-farmer</metric>
    <metric>TS-drawdowns-mean</metric>
    <metric>TS-drawdowns-std</metric>
    <metric>TS-profits-mean</metric>
    <metric>TS-cummulative-wealth-median</metric>
    <metric>TS-total-breaches</metric>
    <metric>TS-boldness</metric>
    <metric>TS-vengefulness</metric>
    <enumeratedValueSet variable="pumping-cap">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-monitoring-capacity">
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fine-magnitude">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-enforcement-cost">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-reputation">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="voluntary-compliance-level">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-breaker-level">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorm?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="monitoring-style">
      <value value="&quot;flat&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-strategy">
      <value value="&quot;random&quot;"/>
      <value value="&quot;risk-based&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graduated-sanctions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SC_PUNJAB_50" repetitions="50" runMetricsEveryStep="false">
    <setup>;;STORE PARAMETERS FOR THIS SIMULATION
let old-pumping-cap pumping-cap
let old-max-monitoring-capacity max-monitoring-capacity
let old-fine-magnitude fine-magnitude
let old-S-enforcement-cost S-enforcement-cost
let old-S-reputation S-reputation
let old-voluntary-compliance-level voluntary-compliance-level
let old-rule-breaker-level rule-breaker-level
let old-metanorm? metanorm?
let old-monitoring-style monitoring-style
let old-enforcement-strategy enforcement-strategy
let old-graduated-sanctions? graduated-sanctions?

;;SETS UP THE GROUNDWATER AND SOCIAL COMPONENTS
set num-farmers 630
SETUP-EXPERIMENT

;;SETS PARAMETERS FOR THE BURN-IN PERIOD
set pumping-cap 0.2
set max-monitoring-capacity 0.1
set fine-magnitude 0.1
set voluntary-compliance-level 0
set rule-breaker-level 0
set metanorm? false
set monitoring-style "flat"
set enforcement-strategy "random"
set graduated-sanctions? false

;;SET ECONOMIC PARAMETERS
set economy? "Punjab: Rice(S), Wheat(W)"

;;RUN THE BURN-IN PERIOD
repeat 100 [go]
reset-ticks
set year 0

;;RELOAD EXPERIMENT PARAMETERS SAVED AT THE START
set pumping-cap old-pumping-cap
set max-monitoring-capacity old-max-monitoring-capacity
set fine-magnitude old-fine-magnitude
set S-enforcement-cost old-S-enforcement-cost
set S-reputation old-S-reputation
set voluntary-compliance-level old-voluntary-compliance-level
set rule-breaker-level old-rule-breaker-level
set metanorm? old-metanorm?
set monitoring-style old-monitoring-style
set enforcement-strategy old-enforcement-strategy
set graduated-sanctions? old-graduated-sanctions?
update-voluntary-compliance
update-rule-breakers</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>TS-norm-strength</metric>
    <metric>TS-compliance</metric>
    <metric>TS-gini-wealth</metric>
    <metric>TS-decision-representative-farmer</metric>
    <metric>TS-drawdowns-mean</metric>
    <metric>TS-drawdowns-std</metric>
    <metric>TS-profits-mean</metric>
    <metric>TS-cummulative-wealth-median</metric>
    <metric>TS-total-breaches</metric>
    <metric>TS-boldness</metric>
    <metric>TS-vengefulness</metric>
    <enumeratedValueSet variable="pumping-cap">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-monitoring-capacity">
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fine-magnitude">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-enforcement-cost">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-reputation">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="voluntary-compliance-level">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-breaker-level">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorm?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="monitoring-style">
      <value value="&quot;flat&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-strategy">
      <value value="&quot;random&quot;"/>
      <value value="&quot;risk-based&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graduated-sanctions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="RB_MDB_50" repetitions="50" runMetricsEveryStep="true">
    <setup>;;STORE PARAMETERS FOR THIS SIMULATION
let old-pumping-cap pumping-cap
let old-max-monitoring-capacity max-monitoring-capacity
let old-fine-magnitude fine-magnitude
let old-S-enforcement-cost S-enforcement-cost
let old-S-reputation S-reputation
let old-voluntary-compliance-level voluntary-compliance-level
let old-rule-breaker-level rule-breaker-level
let old-metanorm? metanorm?
let old-monitoring-style monitoring-style
let old-enforcement-strategy enforcement-strategy
let old-graduated-sanctions? graduated-sanctions?

;;SETS UP THE GROUNDWATER AND SOCIAL COMPONENTS
set num-farmers 10
SETUP-EXPERIMENT

;;SETS PARAMETERS FOR THE BURN-IN PERIOD
set pumping-cap 0.2
set max-monitoring-capacity 0.1
set fine-magnitude 0.1
set voluntary-compliance-level 0
set rule-breaker-level 0
set metanorm? false
set monitoring-style "flat"
set enforcement-strategy "random"
set graduated-sanctions? false

;;SET ECONOMIC PARAMETERS
set economy? "Australia: Cotton(S), Vetch(W)"

;;RUN THE BURN-IN PERIOD
repeat 100 [go]
reset-ticks
set year 0

;;RELOAD EXPERIMENT PARAMETERS SAVED AT THE START
set pumping-cap old-pumping-cap
set max-monitoring-capacity old-max-monitoring-capacity
set fine-magnitude old-fine-magnitude
set S-enforcement-cost old-S-enforcement-cost
set S-reputation old-S-reputation
set voluntary-compliance-level old-voluntary-compliance-level
set rule-breaker-level old-rule-breaker-level
set metanorm? old-metanorm?
set monitoring-style old-monitoring-style
set enforcement-strategy old-enforcement-strategy
set graduated-sanctions? old-graduated-sanctions?
update-voluntary-compliance
update-rule-breakers</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>TS-norm-strength</metric>
    <metric>TS-compliance</metric>
    <metric>TS-gini-wealth</metric>
    <metric>TS-decision-representative-farmer</metric>
    <metric>TS-drawdowns-mean</metric>
    <metric>TS-drawdowns-std</metric>
    <metric>TS-profits-mean</metric>
    <metric>TS-cummulative-wealth-median</metric>
    <metric>TS-total-breaches</metric>
    <metric>TS-boldness</metric>
    <metric>TS-vengefulness</metric>
    <enumeratedValueSet variable="pumping-cap">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-monitoring-capacity">
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fine-magnitude">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-enforcement-cost">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-reputation">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="voluntary-compliance-level">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-breaker-level">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorm?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="monitoring-style">
      <value value="&quot;flat&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-strategy">
      <value value="&quot;random&quot;"/>
      <value value="&quot;risk-based&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graduated-sanctions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="RB_CV_50" repetitions="50" runMetricsEveryStep="false">
    <setup>;;STORE PARAMETERS FOR THIS SIMULATION
let old-pumping-cap pumping-cap
let old-max-monitoring-capacity max-monitoring-capacity
let old-fine-magnitude fine-magnitude
let old-S-enforcement-cost S-enforcement-cost
let old-S-reputation S-reputation
let old-voluntary-compliance-level voluntary-compliance-level
let old-rule-breaker-level rule-breaker-level
let old-metanorm? metanorm?
let old-monitoring-style monitoring-style
let old-enforcement-strategy enforcement-strategy
let old-graduated-sanctions? graduated-sanctions?

;;SETS UP THE GROUNDWATER AND SOCIAL COMPONENTS
set num-farmers 50
SETUP-EXPERIMENT

;;SETS PARAMETERS FOR THE BURN-IN PERIOD
set pumping-cap 0.2
set max-monitoring-capacity 0.1
set fine-magnitude 0.1
set voluntary-compliance-level 0
set rule-breaker-level 0
set metanorm? false
set monitoring-style "flat"
set enforcement-strategy "random"
set graduated-sanctions? false

;;SET ECONOMIC PARAMETERS
set economy? "Central Valley: Almonds(S)"

;;RUN THE BURN-IN PERIOD
repeat 100 [go]
reset-ticks
set year 0

;;RELOAD EXPERIMENT PARAMETERS SAVED AT THE START
set pumping-cap old-pumping-cap
set max-monitoring-capacity old-max-monitoring-capacity
set fine-magnitude old-fine-magnitude
set S-enforcement-cost old-S-enforcement-cost
set S-reputation old-S-reputation
set voluntary-compliance-level old-voluntary-compliance-level
set rule-breaker-level old-rule-breaker-level
set metanorm? old-metanorm?
set monitoring-style old-monitoring-style
set enforcement-strategy old-enforcement-strategy
set graduated-sanctions? old-graduated-sanctions?
update-voluntary-compliance
update-rule-breakers</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>TS-norm-strength</metric>
    <metric>TS-compliance</metric>
    <metric>TS-gini-wealth</metric>
    <metric>TS-decision-representative-farmer</metric>
    <metric>TS-drawdowns-mean</metric>
    <metric>TS-drawdowns-std</metric>
    <metric>TS-profits-mean</metric>
    <metric>TS-cummulative-wealth-median</metric>
    <metric>TS-total-breaches</metric>
    <metric>TS-boldness</metric>
    <metric>TS-vengefulness</metric>
    <enumeratedValueSet variable="pumping-cap">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-monitoring-capacity">
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fine-magnitude">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-enforcement-cost">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-reputation">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="voluntary-compliance-level">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-breaker-level">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorm?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="monitoring-style">
      <value value="&quot;flat&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-strategy">
      <value value="&quot;random&quot;"/>
      <value value="&quot;risk-based&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graduated-sanctions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="RB_PUNJAB_50" repetitions="50" runMetricsEveryStep="false">
    <setup>;;STORE PARAMETERS FOR THIS SIMULATION
let old-pumping-cap pumping-cap
let old-max-monitoring-capacity max-monitoring-capacity
let old-fine-magnitude fine-magnitude
let old-S-enforcement-cost S-enforcement-cost
let old-S-reputation S-reputation
let old-voluntary-compliance-level voluntary-compliance-level
let old-rule-breaker-level rule-breaker-level
let old-metanorm? metanorm?
let old-monitoring-style monitoring-style
let old-enforcement-strategy enforcement-strategy
let old-graduated-sanctions? graduated-sanctions?

;;SETS UP THE GROUNDWATER AND SOCIAL COMPONENTS
set num-farmers 630
SETUP-EXPERIMENT

;;SETS PARAMETERS FOR THE BURN-IN PERIOD
set pumping-cap 0.2
set max-monitoring-capacity 0.1
set fine-magnitude 0.1
set voluntary-compliance-level 0
set rule-breaker-level 0
set metanorm? false
set monitoring-style "flat"
set enforcement-strategy "random"
set graduated-sanctions? false

;;SET ECONOMIC PARAMETERS
set economy? "Punjab: Rice(S), Wheat(W)"

;;RUN THE BURN-IN PERIOD
repeat 100 [go]
reset-ticks
set year 0

;;RELOAD EXPERIMENT PARAMETERS SAVED AT THE START
set pumping-cap old-pumping-cap
set max-monitoring-capacity old-max-monitoring-capacity
set fine-magnitude old-fine-magnitude
set S-enforcement-cost old-S-enforcement-cost
set S-reputation old-S-reputation
set voluntary-compliance-level old-voluntary-compliance-level
set rule-breaker-level old-rule-breaker-level
set metanorm? old-metanorm?
set monitoring-style old-monitoring-style
set enforcement-strategy old-enforcement-strategy
set graduated-sanctions? old-graduated-sanctions?
update-voluntary-compliance
update-rule-breakers</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>TS-norm-strength</metric>
    <metric>TS-compliance</metric>
    <metric>TS-gini-wealth</metric>
    <metric>TS-decision-representative-farmer</metric>
    <metric>TS-drawdowns-mean</metric>
    <metric>TS-drawdowns-std</metric>
    <metric>TS-profits-mean</metric>
    <metric>TS-cummulative-wealth-median</metric>
    <metric>TS-total-breaches</metric>
    <metric>TS-boldness</metric>
    <metric>TS-vengefulness</metric>
    <enumeratedValueSet variable="pumping-cap">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-monitoring-capacity">
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fine-magnitude">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-enforcement-cost">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-reputation">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="voluntary-compliance-level">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-breaker-level">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorm?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="monitoring-style">
      <value value="&quot;flat&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-strategy">
      <value value="&quot;random&quot;"/>
      <value value="&quot;risk-based&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graduated-sanctions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SC_MDB_25" repetitions="25" runMetricsEveryStep="false">
    <setup>;;STORE PARAMETERS FOR THIS SIMULATION
let old-pumping-cap pumping-cap
let old-max-monitoring-capacity max-monitoring-capacity
let old-fine-magnitude fine-magnitude
let old-S-enforcement-cost S-enforcement-cost
let old-S-reputation S-reputation
let old-voluntary-compliance-level voluntary-compliance-level
let old-rule-breaker-level rule-breaker-level
let old-metanorm? metanorm?
let old-monitoring-style monitoring-style
let old-enforcement-strategy enforcement-strategy
let old-graduated-sanctions? graduated-sanctions?

;;SETS UP THE GROUNDWATER AND SOCIAL COMPONENTS
set num-farmers 10
SETUP-EXPERIMENT

;;SETS PARAMETERS FOR THE BURN-IN PERIOD
set pumping-cap 0.2
set max-monitoring-capacity 0.1
set fine-magnitude 0.1
set voluntary-compliance-level 0
set rule-breaker-level 0
set metanorm? false
set monitoring-style "flat"
set enforcement-strategy "random"
set graduated-sanctions? false

;;SET ECONOMIC PARAMETERS
set economy? "Australia: Cotton(S), Vetch(W)"

;;RUN THE BURN-IN PERIOD
repeat 100 [go]
reset-ticks
set year 0

;;RELOAD EXPERIMENT PARAMETERS SAVED AT THE START
set pumping-cap old-pumping-cap
set max-monitoring-capacity old-max-monitoring-capacity
set fine-magnitude old-fine-magnitude
set S-enforcement-cost old-S-enforcement-cost
set S-reputation old-S-reputation
set voluntary-compliance-level old-voluntary-compliance-level
set rule-breaker-level old-rule-breaker-level
set metanorm? old-metanorm?
set monitoring-style old-monitoring-style
set enforcement-strategy old-enforcement-strategy
set graduated-sanctions? old-graduated-sanctions?
update-voluntary-compliance
update-rule-breakers</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>TS-norm-strength</metric>
    <metric>TS-compliance</metric>
    <metric>TS-gini-wealth</metric>
    <metric>TS-decision-representative-farmer</metric>
    <metric>TS-drawdowns-mean</metric>
    <metric>TS-drawdowns-std</metric>
    <metric>TS-profits-mean</metric>
    <metric>TS-cummulative-wealth-median</metric>
    <metric>TS-total-breaches</metric>
    <metric>TS-boldness</metric>
    <metric>TS-vengefulness</metric>
    <enumeratedValueSet variable="pumping-cap">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-monitoring-capacity">
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fine-magnitude">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-enforcement-cost">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-reputation">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="voluntary-compliance-level">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-breaker-level">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorm?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="monitoring-style">
      <value value="&quot;flat&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-strategy">
      <value value="&quot;random&quot;"/>
      <value value="&quot;risk-based&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graduated-sanctions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SC_CV_25" repetitions="25" runMetricsEveryStep="false">
    <setup>;;STORE PARAMETERS FOR THIS SIMULATION
let old-pumping-cap pumping-cap
let old-max-monitoring-capacity max-monitoring-capacity
let old-fine-magnitude fine-magnitude
let old-S-enforcement-cost S-enforcement-cost
let old-S-reputation S-reputation
let old-voluntary-compliance-level voluntary-compliance-level
let old-rule-breaker-level rule-breaker-level
let old-metanorm? metanorm?
let old-monitoring-style monitoring-style
let old-enforcement-strategy enforcement-strategy
let old-graduated-sanctions? graduated-sanctions?

;;SETS UP THE GROUNDWATER AND SOCIAL COMPONENTS
set num-farmers 50
SETUP-EXPERIMENT

;;SETS PARAMETERS FOR THE BURN-IN PERIOD
set pumping-cap 0.2
set max-monitoring-capacity 0.1
set fine-magnitude 0.1
set voluntary-compliance-level 0
set rule-breaker-level 0
set metanorm? false
set monitoring-style "flat"
set enforcement-strategy "random"
set graduated-sanctions? false

;;SET ECONOMIC PARAMETERS
set economy? "Central Valley: Almonds(S)"

;;RUN THE BURN-IN PERIOD
repeat 100 [go]
reset-ticks
set year 0

;;RELOAD EXPERIMENT PARAMETERS SAVED AT THE START
set pumping-cap old-pumping-cap
set max-monitoring-capacity old-max-monitoring-capacity
set fine-magnitude old-fine-magnitude
set S-enforcement-cost old-S-enforcement-cost
set S-reputation old-S-reputation
set voluntary-compliance-level old-voluntary-compliance-level
set rule-breaker-level old-rule-breaker-level
set metanorm? old-metanorm?
set monitoring-style old-monitoring-style
set enforcement-strategy old-enforcement-strategy
set graduated-sanctions? old-graduated-sanctions?
update-voluntary-compliance
update-rule-breakers</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>TS-norm-strength</metric>
    <metric>TS-compliance</metric>
    <metric>TS-gini-wealth</metric>
    <metric>TS-decision-representative-farmer</metric>
    <metric>TS-drawdowns-mean</metric>
    <metric>TS-drawdowns-std</metric>
    <metric>TS-profits-mean</metric>
    <metric>TS-cummulative-wealth-median</metric>
    <metric>TS-total-breaches</metric>
    <metric>TS-boldness</metric>
    <metric>TS-vengefulness</metric>
    <enumeratedValueSet variable="pumping-cap">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-monitoring-capacity">
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fine-magnitude">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-enforcement-cost">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-reputation">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="voluntary-compliance-level">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-breaker-level">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorm?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="monitoring-style">
      <value value="&quot;flat&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-strategy">
      <value value="&quot;random&quot;"/>
      <value value="&quot;risk-based&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graduated-sanctions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SC_PUNJAB_25" repetitions="25" runMetricsEveryStep="false">
    <setup>;;STORE PARAMETERS FOR THIS SIMULATION
let old-pumping-cap pumping-cap
let old-max-monitoring-capacity max-monitoring-capacity
let old-fine-magnitude fine-magnitude
let old-S-enforcement-cost S-enforcement-cost
let old-S-reputation S-reputation
let old-voluntary-compliance-level voluntary-compliance-level
let old-rule-breaker-level rule-breaker-level
let old-metanorm? metanorm?
let old-monitoring-style monitoring-style
let old-enforcement-strategy enforcement-strategy
let old-graduated-sanctions? graduated-sanctions?

;;SETS UP THE GROUNDWATER AND SOCIAL COMPONENTS
set num-farmers 630
SETUP-EXPERIMENT

;;SETS PARAMETERS FOR THE BURN-IN PERIOD
set pumping-cap 0.2
set max-monitoring-capacity 0.1
set fine-magnitude 0.1
set voluntary-compliance-level 0
set rule-breaker-level 0
set metanorm? false
set monitoring-style "flat"
set enforcement-strategy "random"
set graduated-sanctions? false

;;SET ECONOMIC PARAMETERS
set economy? "Punjab: Rice(S), Wheat(W)"

;;RUN THE BURN-IN PERIOD
repeat 100 [go]
reset-ticks
set year 0

;;RELOAD EXPERIMENT PARAMETERS SAVED AT THE START
set pumping-cap old-pumping-cap
set max-monitoring-capacity old-max-monitoring-capacity
set fine-magnitude old-fine-magnitude
set S-enforcement-cost old-S-enforcement-cost
set S-reputation old-S-reputation
set voluntary-compliance-level old-voluntary-compliance-level
set rule-breaker-level old-rule-breaker-level
set metanorm? old-metanorm?
set monitoring-style old-monitoring-style
set enforcement-strategy old-enforcement-strategy
set graduated-sanctions? old-graduated-sanctions?
update-voluntary-compliance
update-rule-breakers</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>TS-norm-strength</metric>
    <metric>TS-compliance</metric>
    <metric>TS-gini-wealth</metric>
    <metric>TS-decision-representative-farmer</metric>
    <metric>TS-drawdowns-mean</metric>
    <metric>TS-drawdowns-std</metric>
    <metric>TS-profits-mean</metric>
    <metric>TS-cummulative-wealth-median</metric>
    <metric>TS-total-breaches</metric>
    <metric>TS-boldness</metric>
    <metric>TS-vengefulness</metric>
    <enumeratedValueSet variable="pumping-cap">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-monitoring-capacity">
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fine-magnitude">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-enforcement-cost">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-reputation">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="voluntary-compliance-level">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-breaker-level">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorm?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="monitoring-style">
      <value value="&quot;flat&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-strategy">
      <value value="&quot;random&quot;"/>
      <value value="&quot;risk-based&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graduated-sanctions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="RB_MDB_25" repetitions="25" runMetricsEveryStep="false">
    <setup>;;STORE PARAMETERS FOR THIS SIMULATION
let old-pumping-cap pumping-cap
let old-max-monitoring-capacity max-monitoring-capacity
let old-fine-magnitude fine-magnitude
let old-S-enforcement-cost S-enforcement-cost
let old-S-reputation S-reputation
let old-voluntary-compliance-level voluntary-compliance-level
let old-rule-breaker-level rule-breaker-level
let old-metanorm? metanorm?
let old-monitoring-style monitoring-style
let old-enforcement-strategy enforcement-strategy
let old-graduated-sanctions? graduated-sanctions?

;;SETS UP THE GROUNDWATER AND SOCIAL COMPONENTS
set num-farmers 10
SETUP-EXPERIMENT

;;SETS PARAMETERS FOR THE BURN-IN PERIOD
set pumping-cap 0.2
set max-monitoring-capacity 0.1
set fine-magnitude 0.1
set voluntary-compliance-level 0
set rule-breaker-level 0
set metanorm? false
set monitoring-style "flat"
set enforcement-strategy "random"
set graduated-sanctions? false

;;SET ECONOMIC PARAMETERS
set economy? "Australia: Cotton(S), Vetch(W)"

;;RUN THE BURN-IN PERIOD
repeat 100 [go]
reset-ticks
set year 0

;;RELOAD EXPERIMENT PARAMETERS SAVED AT THE START
set pumping-cap old-pumping-cap
set max-monitoring-capacity old-max-monitoring-capacity
set fine-magnitude old-fine-magnitude
set S-enforcement-cost old-S-enforcement-cost
set S-reputation old-S-reputation
set voluntary-compliance-level old-voluntary-compliance-level
set rule-breaker-level old-rule-breaker-level
set metanorm? old-metanorm?
set monitoring-style old-monitoring-style
set enforcement-strategy old-enforcement-strategy
set graduated-sanctions? old-graduated-sanctions?
update-voluntary-compliance
update-rule-breakers</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>TS-norm-strength</metric>
    <metric>TS-compliance</metric>
    <metric>TS-gini-wealth</metric>
    <metric>TS-decision-representative-farmer</metric>
    <metric>TS-drawdowns-mean</metric>
    <metric>TS-drawdowns-std</metric>
    <metric>TS-profits-mean</metric>
    <metric>TS-cummulative-wealth-median</metric>
    <metric>TS-total-breaches</metric>
    <metric>TS-boldness</metric>
    <metric>TS-vengefulness</metric>
    <enumeratedValueSet variable="pumping-cap">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-monitoring-capacity">
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fine-magnitude">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-enforcement-cost">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-reputation">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="voluntary-compliance-level">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-breaker-level">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorm?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="monitoring-style">
      <value value="&quot;flat&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-strategy">
      <value value="&quot;random&quot;"/>
      <value value="&quot;risk-based&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graduated-sanctions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="RB_CV_25" repetitions="25" runMetricsEveryStep="false">
    <setup>;;STORE PARAMETERS FOR THIS SIMULATION
let old-pumping-cap pumping-cap
let old-max-monitoring-capacity max-monitoring-capacity
let old-fine-magnitude fine-magnitude
let old-S-enforcement-cost S-enforcement-cost
let old-S-reputation S-reputation
let old-voluntary-compliance-level voluntary-compliance-level
let old-rule-breaker-level rule-breaker-level
let old-metanorm? metanorm?
let old-monitoring-style monitoring-style
let old-enforcement-strategy enforcement-strategy
let old-graduated-sanctions? graduated-sanctions?

;;SETS UP THE GROUNDWATER AND SOCIAL COMPONENTS
set num-farmers 50
SETUP-EXPERIMENT

;;SETS PARAMETERS FOR THE BURN-IN PERIOD
set pumping-cap 0.2
set max-monitoring-capacity 0.1
set fine-magnitude 0.1
set voluntary-compliance-level 0
set rule-breaker-level 0
set metanorm? false
set monitoring-style "flat"
set enforcement-strategy "random"
set graduated-sanctions? false

;;SET ECONOMIC PARAMETERS
set economy? "Central Valley: Almonds(S)"

;;RUN THE BURN-IN PERIOD
repeat 100 [go]
reset-ticks
set year 0

;;RELOAD EXPERIMENT PARAMETERS SAVED AT THE START
set pumping-cap old-pumping-cap
set max-monitoring-capacity old-max-monitoring-capacity
set fine-magnitude old-fine-magnitude
set S-enforcement-cost old-S-enforcement-cost
set S-reputation old-S-reputation
set voluntary-compliance-level old-voluntary-compliance-level
set rule-breaker-level old-rule-breaker-level
set metanorm? old-metanorm?
set monitoring-style old-monitoring-style
set enforcement-strategy old-enforcement-strategy
set graduated-sanctions? old-graduated-sanctions?
update-voluntary-compliance
update-rule-breakers</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>TS-norm-strength</metric>
    <metric>TS-compliance</metric>
    <metric>TS-gini-wealth</metric>
    <metric>TS-decision-representative-farmer</metric>
    <metric>TS-drawdowns-mean</metric>
    <metric>TS-drawdowns-std</metric>
    <metric>TS-profits-mean</metric>
    <metric>TS-cummulative-wealth-median</metric>
    <metric>TS-total-breaches</metric>
    <metric>TS-boldness</metric>
    <metric>TS-vengefulness</metric>
    <enumeratedValueSet variable="pumping-cap">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-monitoring-capacity">
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fine-magnitude">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-enforcement-cost">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-reputation">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="voluntary-compliance-level">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-breaker-level">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorm?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="monitoring-style">
      <value value="&quot;flat&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-strategy">
      <value value="&quot;random&quot;"/>
      <value value="&quot;risk-based&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graduated-sanctions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="RB_PUNJAB_25" repetitions="25" runMetricsEveryStep="false">
    <setup>;;STORE PARAMETERS FOR THIS SIMULATION
let old-pumping-cap pumping-cap
let old-max-monitoring-capacity max-monitoring-capacity
let old-fine-magnitude fine-magnitude
let old-S-enforcement-cost S-enforcement-cost
let old-S-reputation S-reputation
let old-voluntary-compliance-level voluntary-compliance-level
let old-rule-breaker-level rule-breaker-level
let old-metanorm? metanorm?
let old-monitoring-style monitoring-style
let old-enforcement-strategy enforcement-strategy
let old-graduated-sanctions? graduated-sanctions?

;;SETS UP THE GROUNDWATER AND SOCIAL COMPONENTS
set num-farmers 630
SETUP-EXPERIMENT

;;SETS PARAMETERS FOR THE BURN-IN PERIOD
set pumping-cap 0.2
set max-monitoring-capacity 0.1
set fine-magnitude 0.1
set voluntary-compliance-level 0
set rule-breaker-level 0
set metanorm? false
set monitoring-style "flat"
set enforcement-strategy "random"
set graduated-sanctions? false

;;SET ECONOMIC PARAMETERS
set economy? "Punjab: Rice(S), Wheat(W)"

;;RUN THE BURN-IN PERIOD
repeat 100 [go]
reset-ticks
set year 0

;;RELOAD EXPERIMENT PARAMETERS SAVED AT THE START
set pumping-cap old-pumping-cap
set max-monitoring-capacity old-max-monitoring-capacity
set fine-magnitude old-fine-magnitude
set S-enforcement-cost old-S-enforcement-cost
set S-reputation old-S-reputation
set voluntary-compliance-level old-voluntary-compliance-level
set rule-breaker-level old-rule-breaker-level
set metanorm? old-metanorm?
set monitoring-style old-monitoring-style
set enforcement-strategy old-enforcement-strategy
set graduated-sanctions? old-graduated-sanctions?
update-voluntary-compliance
update-rule-breakers</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>TS-norm-strength</metric>
    <metric>TS-compliance</metric>
    <metric>TS-gini-wealth</metric>
    <metric>TS-decision-representative-farmer</metric>
    <metric>TS-drawdowns-mean</metric>
    <metric>TS-drawdowns-std</metric>
    <metric>TS-profits-mean</metric>
    <metric>TS-cummulative-wealth-median</metric>
    <metric>TS-total-breaches</metric>
    <metric>TS-boldness</metric>
    <metric>TS-vengefulness</metric>
    <enumeratedValueSet variable="pumping-cap">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-monitoring-capacity">
      <value value="0.1"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="fine-magnitude">
      <value value="0.1"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-enforcement-cost">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="S-reputation">
      <value value="0.2"/>
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="voluntary-compliance-level">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rule-breaker-level">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorm?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="monitoring-style">
      <value value="&quot;flat&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-strategy">
      <value value="&quot;random&quot;"/>
      <value value="&quot;risk-based&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="graduated-sanctions?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
