# helpful value swapping functions. 

swaps = c(
    from1 = 'to1',
    from2 = 'to2'
)

swap = function(x) sapply(x, function(ix){
    if(ix %in% names(swaps)){
        return (swaps[[ix]])
    } else if(ix %in% swaps){
        return(names(swaps)[which(swaps == ix)])
    } else {
        return(tools::toTitleCase(ix))
    }
})

swapnames = function(x){
    names(x) = swap(names(x))
    return(x)
}

