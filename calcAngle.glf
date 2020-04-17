package require PWI_Glyph

set retValue [source [file join [file dirname [info script]] "/Users/ehereth/Projects/Glyph/GridCoordEnum/pwio.glf"]]

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

# vim: set ft=tcl:
