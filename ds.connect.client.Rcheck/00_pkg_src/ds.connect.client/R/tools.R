is.class.type.correct <- function(class.type)
{
  valid.types <- c("NULL","character","complex","factor","double","expression","integer",
                   "list","mabrix","logical","numeric","single","raw","vector","S4","NULL",
                   "function","externalptr","environment", "RangedSummarizedExperiment", "SummarizedExperiment",
                   "ExpressionSet")
  outcome <- FALSE
  if (is.character(class.type))
  {
    if(class.type %in% valid.types)
    {
      outcome <- TRUE
    }
  }
  
  return(outcome)
}

is.character.argument.correct <- function(character.value)
{
  outcome <- FALSE
  
  if (is.character(character.value))
  {
    if (nchar(character.value) > 0)
    {
      outcome <- TRUE
    }
  }
  
  return(outcome)
  
}

is.value.for.assignment.correct <- function(value)
{
  outcome <- is.character.argument.correct(value)
  
  if (is.call(value) || is.symbol(value))
  {
    outcome <- TRUE
  }
  return(outcome)
}

is.vector.argument.correct <- function(argument.value)
{
  outcome <- FALSE
 
  if (!is.null(argument.value))
  {
    is.null(argument.value)
    if (length(argument.value) >  0)
    {
      outcome <- TRUE
    }
  }
  return(outcome)
}