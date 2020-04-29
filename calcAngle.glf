package require PWI_Glyph

# Load math constants for pi
package require math::constants

set retValue [source [file join [file dirname [info script]] "/Users/ehereth/Projects/Glyph/GridCoordEnum/pwio.glf"]]

# This procedure calculates the average of all points provided to it.
proc getAveragePoint { points } {
  set nPoints [llength $points]
  set avg [pwu::Vector3 zero]
  foreach p $points {
    set pt [pwu::Vector3 set [lindex $p 0] [lindex $p 1] [lindex $p 2]]
    set avg [pwu::Vector3 add $avg $pt]
  }

  #set avg [pwu::Vector3 scale $avg [expr 1.0/$nPoints]]
  set avg [pwu::Vector3 divide $avg $nPoints]

  return [list [pwu::Vector3 x $avg] [pwu::Vector3 y $avg] [pwu::Vector3 z $avg]]
}

# This is for debugging connectivity mostly.
proc labelMesh { domains} {
  puts "Labeling mesh using: $domains."

  # Using the pwio library internally, this may not be efficient if this is
  # done in many other procedures too.
  pwio::beginIO $domains

  # Create coordinate/node labels.
  set coordCount [pwio::getCoordCount]
  puts "\tthere are $coordCount total coordinates..."

  for {set p 1} {$p <= $coordCount} {incr p} {
    set coord [pwio::getCoord $p]
    #puts "coord: $coord"
    #puts "\tcoordinate $p: [pwio::utils::coordToPtString $coord]"
    set pt [pw::Grid getPoint $coord]
    pwio::utils::labelPt $p $pt
  }

  # Create edge labels.
  set edgeToNode [dict create]
  set edgeToNodeHash [dict create]

  # Create edge-to-node connectivity and hash.
  lassign [createEdgeToNodeConnectivity $domains] edgeToNode edgeToNodeHash

  set edgeCount [dict size $edgeToNode]
  puts "\tthere are $edgeCount total edges..."

  dict for {edge coodPair} $edgeToNode {
    set edgePoints [list]
    foreach p $coodPair {
      set coord [pwio::getCoord $p]
      lappend edgePoints [pw::Grid getPoint $coord]
    }
    set eAvgPt [getAveragePoint $edgePoints]
    #puts "\t\taverage point: $eAvgPt"
    pwio::utils::labelPt $edge $eAvgPt
  }

  set cellCount [pwio::getCellCount]
  puts "\tthere are $cellCount total facets..."

  # Create facet labels.
  for {set f 1} {$f <= $cellCount} {incr f} {
    #puts "facet $f is a [pwio::getCellType $f]"
    set facet [pwio::getCell $f]
    set facetPoints [list]
    foreach p $facet {
      set coord [pwio::getCoord $p]
      lappend facetPoints [pw::Grid getPoint $coord]
    }

    #puts "\tpoints: $facetPoints"
    set fAvgPt [getAveragePoint $facetPoints]
    #puts "\t\taverage point: $fAvgPt"
    pwio::utils::labelPt $f $fAvgPt
  }

  pwio::endIO
}

# This creates both a hash/lookup table and a edge-to-node connectivity for all
# the edges contained in the domain(s) provided in _facet_ order.
proc createEdgeToNodeConnectivity { domains } {
  #puts "Creating edge-to-node connectivity using: $domains."

  # Using the pwio library internally, this may not be efficient if this is
  # done in many other procedures too.
  pwio::beginIO $domains

  set edgeToNode [dict create]
  set edgeToNodeHash [dict create]
  set nEdges 1

  set cellCount [pwio::getCellCount]

  # Loop over facets and extract edges.
  for {set f 1} {$f <= $cellCount} {incr f} {
    set facet [pwio::getCell $f]
    # This is in 'min' order to make it easier to identify unique edges.
    set edges [pwio::getCellEdges $f cellVarName 1 revVarName]
    #puts "facet $f: edges: $edges"

    foreach edge $edges {
      # Create unique edge identifier.
      set edgeId "[lindex $edge 0]-[lindex $edge 1]"
      # If this unique edge does not yet exist, create its hash and nade map.
      if { ![dict exists $edgeToNodeHash $edgeId] } {
        dict set edgeToNodeHash $edgeId $nEdges
        dict set edgeToNode $nEdges [list [lindex $edge 0] [lindex $edge 1]]
        set nEdges [expr $nEdges + 1]
      }
    }
  }

  pwio::endIO

  return [list $edgeToNode $edgeToNodeHash]
}

proc createEdgeToCellConnectivity { domains } {
  #puts "Creating edge-to-cell connectivity using: $domains."

  # Create unique edge maps which are needed below.
  lassign [createEdgeToNodeConnectivity $domains] edgeToNode edgeToNodeHash
  #puts "edgeToNode: $edgeToNode"

  # Using the pwio library internally, this may not be efficient if this is
  # done in many other procedures too.
  pwio::beginIO $domains

  set edgeToCell [dict create]

  set cellCount [pwio::getCellCount]
  #puts "\tthere are $cellCount total facets..."

  # Loop over facets and identify neighbor facets
  for {set f 1} {$f <= $cellCount} {incr f} {
    set facet [pwio::getCell $f]
    # This is in 'min' order to make it easier to identify unique edges.
    set edges [pwio::getCellEdges $f cellVarName 1 revVarName]
    #puts "\t\t\facet $f: $facet has (min) edges: $edges"
    #puts "\t\t\tcellVarName: $cellVarName, revVarName: $revVarName"

    # Loop over all remaining facets to identify which ones share an edge.
    for {set ff [expr $f + 1]} {$ff <= $cellCount} {incr ff} {
      set facet2 [pwio::getCell $ff]
    # This is in 'min' order to make it easier to identify unique edges.
      set edges2 [pwio::getCellEdges $ff cellVarName 1 revVarName]

      # Loop over edges of original facet and compare with edges owned by
      # current facet.
      foreach edge $edges {
        #puts "edge: $edge"
        set edgeId [dict get $edgeToNodeHash "[lindex $edge 0]-[lindex $edge 1]"]
        foreach edge2 $edges2 {
          set edge2Id [dict get $edgeToNodeHash "[lindex $edge2 0]-[lindex $edge2 1]"]
          #puts "edge2: $edge2"

          if { $edgeId == $edge2Id} {
            #puts "Found identical edges..."
            #puts "\tedgeId: $edgeId and edge2Id: $edge2Id"
            # Build up edge-to-cell connectivity dictionary.
            dict set edgeToCell $edgeId [list $f $ff]
          }
        }
      }
    }
  }

  puts "edgeToCell: $edgeToCell"

  pwio::endIO

  return $edgeToCell
}

proc createCellToCellConnectivity { domains } {
  #puts "Creating cell-to-cell connectivity using: $domains."

  # Start with edge-to-cell connectivity.
  set edgeToCell [ createEdgeToCellConnectivity $domains ]

  set cellToCell [dict create]

  dict for {edge cellPair} $edgeToCell {
    #puts "edge: $edge contains the cell pair: $cellPair"
    set cell1 [lindex $cellPair 0]
    set cell2 [lindex $cellPair 1]
    dict lappend cellToCell $cell1 $cell2
    dict lappend cellToCell $cell2 $cell1
  }

  #puts "cellToCell: $cellToCell"
  return $cellToCell
}

proc edgeToVector { edgeToNodeName edge reverse } {

  # This implies that $edgeToNode exists in the callstack/scope of the
  # procedure that calls this. I do not like TCL, but this seems to be the way
  # to do this.
  upvar 1 $edgeToNodeName edgeToNode

  set coord1 [pwio::getCoord [lindex [dict get $edgeToNode $edge] 0]]
  set coord2 [pwio::getCoord [lindex [dict get $edgeToNode $edge] 1]]

  #puts "coord1: $coord1, coord2: $coord2"

  set xyz1 [pw::Grid getXYZ $coord1]
  set xyz2 [pw::Grid getXYZ $coord2]

  #puts "xyz1: $xyz1, xyz2: $xyz2"

  set vector [pwu::Vector3 set [expr {[lindex $xyz2 0] - [lindex $xyz1 0]}] [expr {[lindex $xyz2 1] - [lindex $xyz1 1]}] [expr {[lindex $xyz2 2] - [lindex $xyz1 2]}]]

  if { $reverse } { set vector [pwu::Vector3 scale $vector -1.0]}

  return $vector
}

# This procedure accepts as input the common edge between the two facets in
# question and computes the angle between them.
proc getAngleBetweenFaces { edgeToNodeHashName edgeToNodeName edgeToCellName commonEdge } {

  # Define pi
  math::constants::constants pi

  # This implies that $edgeToNode, $edgeToCell both exist in the
  # callstack/scope of the procedure that calls this. I do not like TCL, but
  # this seems to be the way to do this.
  upvar 1 $edgeToNodeHashName edgeToNodeHash
  upvar 1 $edgeToNodeName edgeToNode
  upvar 1 $edgeToCellName edgeToCell

  set vectorCount 1
  set vectors [list [edgeToVector $edgeToNodeName $commonEdge 0]]

  # The first coordinate/node of the common edge is used as the base for all
  # vectors used in the calculation.
  set anchorCoord [lindex [dict get $edgeToNode $commonEdge] 0]

  set facets [dict get $edgeToCell $commonEdge]
  #puts "facets: $facets"

  if { [llength $facets] != 2 } {
    puts "warning: found boundary edge"
    continue
  }

  foreach facetIndex $facets {
    set facet [pwio::getCell $facetIndex]
    # This is in 'min' order to make it easier to identify unique edges.
    set edges [pwio::getCellEdges $facetIndex cellVarName 1 revVarName]
    #puts "facet $facet: edges: $edges"
    foreach edge $edges {
      # Create unique edge identifier.
      set edgeId "[lindex $edge 0]-[lindex $edge 1]"
      set currentEdge [dict get $edgeToNodeHash $edgeId]
      #puts "\tfacet: $facet: current edgeId: $edgeId"

      # Make sure this is not the common edge and that it shares the anchor
      # coordinate/node with the common edge.
      if { $commonEdge != $currentEdge && \
           ( $anchorCoord == [lindex [dict get $edgeToNode $currentEdge] 0] || \
             $anchorCoord == [lindex [dict get $edgeToNode $currentEdge] 1] ) } {
        set reverse 0
        if { $anchorCoord == [lindex [dict get $edgeToNode $currentEdge] 1] } { set reverse 1 }
        #puts "\t\tedge: $currentEdge, reverse: $reverse"
        lappend vectors [edgeToVector $edgeToNodeName $currentEdge $reverse]
        incr vectorCount
      }
    }
  }

  #puts "We have $vectorCount vectors: $vectors"

  # Assert that there are only three vectors.
  pwio::utils::assert "\"3\" == \"[llength $vectors]\"" "Error: incorrect number of vectors"

  # Now, calculate angle between the adjacent faces.
  set cross1 [pwu::Vector3 cross [lindex $vectors 0] [lindex $vectors 1]]
  set cross2 [pwu::Vector3 cross [lindex $vectors 0] [lindex $vectors 2]]

  set vector1Len [pwu::Vector3 length $cross1]
  set vector2Len [pwu::Vector3 length $cross2]

  set dot [pwu::Vector3 dot $cross1 $cross2]

  set arg [expr {$dot/[expr $vector1Len * $vector2Len]}]
  # Make sure this is in the range of [-1.0, 1.0] so that acos does not fail.
  set arg [expr {min(max($arg, -1.0), 1.0)}]

  #puts "vector1Len: $vector1Len, vector2Len: $vector2Len"
  #puts "cross1: $cross1, cross2: $cross2"
  #puts "dot cross1 x cross2: $dot"

  #set angle [expr 180 * [expr {acos($dot/[expr $vector1Len * $vector2Len])} ]/[expr {atan(1) * 4}]]
  #set angle [expr 180 * [expr {acos($arg)} ]/[expr {atan(1) * 4}]]
  set angle [expr 180 * [expr {acos($arg)} ]/$pi]
  puts "Angle between edge: $commonEdge and facets: $facets is: $angle"
}

# Create a domain selection mask.
set mask [pw::Display createSelectionMask -requireDomain {}];

#
# Use selected domain or prompt user for selection if nothing is selected at
# run time.
#
if { !([pw::Display getSelectedEntities -selectionmask $mask selection]) } {
  # No domain was selected at runtime; prompt for one now.

# Create a domain selection dialog.
if { ![pw::Display selectEntities \
       -selectionmask $mask \
       -description "Select domain(s) to use." \
     selection] } {

    puts "Error: Unsuccessfully selected domain(s)... exiting"
    exit
  }

}

puts "Selected $selection(Domains) for use."

labelMesh $selection(Domains)

lassign [ createEdgeToNodeConnectivity $selection(Domains) ] edgeToNode edgeToNodeHash

#puts "edgeToNode: $edgeToNode"

set edgeToCell [ createEdgeToCellConnectivity $selection(Domains) ]

#puts "edgeToCell: $edgeToCell"

#set cellToCell [ createCellToCellConnectivity $selection(Domains) ]

#puts "cellToCell: $cellToCell"

set edgeCount [dict size $edgeToNode]
for {set edge 1} {$edge <= $edgeCount} {incr edge} {
  #puts "\t\tcalling getAngleBetweenFaces on edge $edge"
  #puts "\t\t\tedgeToNode(edge): [dict get $edgeToNode $edge]"
  if {[dict exist $edgeToCell $edge]} {
    #puts "\t\t\tedgeToCell(edge): [dict get $edgeToCell $edge]"
    getAngleBetweenFaces "edgeToNodeHash" "edgeToNode" "edgeToCell" $edge
  }
}

# vim: set ft=tcl:
