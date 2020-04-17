package require PWI_Glyph

set retValue [source [file join [file dirname [info script]] "/Users/ehereth/Projects/Glyph/GridCoordEnum/pwio.glf"]]

# This procedure calculates the average of all points provided to it.
proc getAveragePoint { points } {
  set nPoints [llength $points]
  set avg [pwu::Vector3 zero]
  foreach p $points {
    set pt [pwu::Vector3 set [lindex $p 0] [lindex $p 1] [lindex $p 2]]
    set avg [pwu::Vector3 add $avg $pt]
  }

  set avg [pwu::Vector3 scale $avg [expr 1.0/$nPoints]]

  return [list [pwu::Vector3 x $avg] [pwu::Vector3 y $avg] [pwu::Vector3 z $avg]]
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

  #puts "edgeToCell: $edgeToCell"

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

set edgeToCell [ createEdgeToCellConnectivity $selection(Domains) ]

puts "edgeToCell: $edgeToCell"

# vim: set ft=tcl:
