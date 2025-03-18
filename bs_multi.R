bs_accordion_multi = function(X, multi=TRUE, open=1) {
  for(i in 1:length(X$children))
  {
    if(multi)
      # Remove 'data-parent' attribute so multiple panels can be open at once
      X$children[[i]]$children[[1]]$attribs$`data-parent` <- NULL
    
    # Remove 'in' class to prevent *any* panel from starting as open
    classAttribs <- which(names(X$children[[i]]$children[[2]]$attribs) == "class")
    for(j in classAttribs)
    {
      if(X$children[[i]]$children[[2]]$attribs[j]=="in")
      {
        X$children[[i]]$children[[2]]$attribs[j] <- NULL
      }
    }
    
    if(i %in% open)
    {
      # Add 'in' class (back) to panels selected to start as open
      X$children[[i]]$children[[2]]$attribs <- append(X$children[[i]]$children[[2]]$attribs, list(class="in"))
    }
    else
    {
      # Add 'collapsed' class to panels slected to start as closed, so css rules work properly
      X$children[[i]]$children[[1]]$attribs <- append(X$children[[i]]$children[[1]]$attribs, list(class="collapsed"))
    }
  }
  
  X
  
}