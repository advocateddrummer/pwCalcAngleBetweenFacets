package require PWI_Glyph

set retValue [source [file join [file dirname [info script]] "/Users/ehereth/Projects/Glyph/GridCoordEnum/pwio.glf"]]


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

  # Using the pwio library internally, this may not be efficient if this is
  # done in many other procedures too.
  pwio::beginIO $domains

  set edgeToCell [dict create]
  set nEdges 0

  set cellCount [pwio::getCellCount]
  #puts "\tthere are $cellCount total facets..."

  # Loop over facets and identify neighbor facets
  for {set f 1} {$f <= $cellCount} {incr f} {
    set facet [pwio::getCell $f]
    #set edges [pwio::getCellEdges $f]
    #puts "\t\tfacet $f: $facet has edges: $edges"
    set minEdges [pwio::getCellEdges $f cellVarName 1 revVarName]
    #puts "\t\t\facet $f: $facet has (min) edges: $minEdges"
    #puts "\t\t\tcellVarName: $cellVarName, revVarName: $revVarName"

    # Loop over all remaining facets to identify which ones share an edge.
    for {set ff [expr $f + 1]} {$ff <= $cellCount} {incr ff} {
      set facet2 [pwio::getCell $ff]
      set minEdges2 [pwio::getCellEdges $ff cellVarName 1 revVarName]

      # Loop over edges of original facet and compare with edges owned by
      # current facet.
      foreach edge $minEdges {
        #puts "edge: $edge"
        foreach edge2 $minEdges2 {
          #puts "edge2: $edge2"

          # There must be a better way to do this.
          if { [lindex $edge 0] == [lindex $edge2 0] && [lindex $edge 1] == [lindex $edge2 1] } {
            #puts "Found identical edges..."
            # Build up edge-to-cell connectivity dictionary.
            dict set edgeToCell $nEdges [list $f $ff]
            set nEdges [expr $nEdges + 1]
          }
        }
      }
    }
  }

  #puts "edgeToCell: $edgeToCell"

  pwio::endIO

  return $edgeToCell
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
