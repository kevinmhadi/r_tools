
### DATA TABLE HELPERS

fullSD = function(.SD, .BY)
{
    cbind(copy(.SD), data.table(t(unlist(.BY))))
}

fullSD2 = function(p_env = parent.env(environment())) {
    expr = expression(cbind(.SD, data.table(t(unlist(.BY)))))
    eval(expr, envir = parent.env(environment()))
}

    

v = function (expr) {
    base::eval(expr, sys.frame(4), sys.frame(1))
}



d_evc = function(string_vec, calling_env = parent.frame()) {
    eval(dc(string_vec), envir = calling_env)
}

d_evl = function(string_vec, calling_env = parent.frame()) {
    eval(dl(string_vec), envir = calling_env)
}




dc = function(string_vec, q = T) {
    ez_eval(string_vec, c = T, q = T)
}

dl = function(string_vec, c = F,  q = F) {
    ez_eval(string_vec, c = F, q = F)
}


## allows so if you have a bunch of data table columns to parse 
ez_eval = function(string_vec, c = T, list = !c, quotes = T) {
    op_string_c = "c("
    op_string_l = "list("
    if (quotes) {
        q  = "\""
    }
    else
        q = NULL
    ## cat('current frame is', sys.nframe(), "\n")
    ## cat('parent frame is', sys.parent(), "\n")
    c_cmd = parse(text = (paste0(op_string_c, paste0(q, string_vec, q, collapse = ", "), ")")))
    list_cmd = parse(text = (paste0(op_string_l, paste0(q, string_vec, q, collapse = ", "), ")")))
    lst_args = as.list(match.call())
    c_arg = eval(lst_args$c)
    l_arg = eval(lst_args$list)
    c_arg_cond = tryCatch(! is.null(c_arg) & c_arg, error = function(e) FALSE)
    l_arg_cond = tryCatch(! is.null(l_arg) & l_arg, error = function(e) FALSE)
    if (length(l_arg_cond) == 0) {
        l_arg_cond = FALSE
    }
    ## if (is.null(l_arg)) {
    ##     if (is.null(c_arg)) {
    ##         is_c = TRUE
    ##         is_l = FALSE
    ##     } else {
    ##         if (c_arg) {
    ##             is_c = TRUE
    ##             is_l = FALSE
    ##         } else {
    ##             is_c = FALSE
    ##             is_l = TRUE
    ##         }
    ##     }
    ## } else {
    ##     if (l_arg) {
    ##         is_l = TRUE
    ##     }
    ##     if (!is.null(c_arg)) {
    ##         if (c_arg) {
    ##             is_c = TRUE
    ##         } else {
    ##             is_c = FALSE
    ##         }
    ##     } else {
    ##         is_c = FALSE
    ##     }
    ## }
    ## if (is_c) {
    ##     if (is_l) {
    ##         (c_cmd)
    ##         (list_cmd)
    ##     }
    ##     c_cmd
    ## } else if (is_l) {
    ##     (list_cmd)
    ## }
        
    ## browser()
    if (all(is.null(c(c_arg, l_arg))))
        (c_cmd)
    else if(c_arg_cond & l_arg_cond) {
        (c_cmd)
        (list_cmd)
    }
    else if (l_arg_cond)
        (list_cmd)
    else if (is.null(c_arg) & ! l_arg_cond)
        (c_cmd)
    else if (! c_arg_cond) {
        ## eval((list_cmd), parent.frame(1), parent.frame(2))
        (list_cmd)
    }
    else
        (c_cmd)
}


ez_string = function(string_vec, c = T, list = !c, quotes = T, ws = "\n") {
    ws = paste0(",", ws)
    op_string_c = "c("
    op_string_l = "list("
    if (quotes) {
        q  = "\""
    }
    else
        q = NULL
    c_cmd = expression(cat(paste0(op_string_c, paste0(q, string_vec, q, collapse = ws), ")\n")))
    list_cmd = expression(cat(paste0(op_string_l, paste0(q, string_vec, q, collapse = ws), ")\n")))
    lst_args = as.list(match.call())
    c_arg = eval(lst_args$c)
    l_arg = eval(lst_args$list)
    c_arg_cond = tryCatch(! is.null(c_arg) & c_arg, error = function(e) FALSE)
    l_arg_cond = tryCatch(! is.null(l_arg) & l_arg, error = function(e) FALSE)
    if (all(is.null(c(c_arg, l_arg))))
        eval(c_cmd)
    else if(c_arg_cond & l_arg_cond) {
        eval(c_cmd)
        eval(list_cmd)
    }
    else if (l_arg_cond)
        eval(list_cmd)
    else if (is.null(c_arg) & ! l_arg_cond)
        eval(c_cmd)
    else if (! c_arg_cond)
        eval(list_cmd)
    else
        eval(c_cmd)
}


#' Data table merging wrappers

### replace
merge.repl = function(dt.x,
                      dt.y,
                      replace_in_x = TRUE,
                      suffix = NULL,
                      sep = "_",
                      replace_NA = TRUE,
                      force_y = TRUE,
                      ...)
{
    arg_lst = as.list(match.call())
    by.y = eval(arg_lst$by.y)
    by.x = eval(arg_lst$by.x)
    by = eval(arg_lst$by)
    if (is.null(by.y) & is.null(by.x) & is.null(by)) {
        k.x = key(dt.x)
        k.y = key(dt.y)
        if (is.null(k.x) | is.null(k.y) || (k.x != k.y)) {
            stop("neither by.x/by.y  nor by are supplied, keys of dt.x and dt.y must be identical and non NULL")
        }
        x.cols = setdiff(names(dt.x), k.x)
        y.cols = setdiff(names(dt.y), k.y)
    } else if (!is.null(by.x) & !is.null(by.y)) {
        x.cols = setdiff(names(dt.x), by.x)
        y.cols = setdiff(names(dt.y), by.y)
    } else if (!is.null(by)) {
        x.cols = setdiff(names(dt.x), by)
        y.cols = setdiff(names(dt.y), by)
        if (length(x.cols) == 0 | length(y.cols) == 0) {
            stop("column ", by, "does not exist in one of the tables supplied \nCheck the column names")
        }
    }
    these_cols = intersect(x.cols, y.cols)
    if (replace_in_x) {
        if (! replace_NA) {            
            dt.x.tmp = copy(dt.x)[, eval(dc(these_cols)) := NULL]
            dt.repl = merge(dt.x.tmp, dt.y, all.x = TRUE, ...)
        } else {
            dt.repl = merge(dt.x, dt.y, all.x = TRUE, ...)
            lapply(these_cols, function(this_col)
            {
                x_cname = paste0(this_col, ".x")
                y_cname = paste0(this_col, ".y")
                x_col = as.data.frame(dt.repl)[, x_cname]
                y_col = as.data.frame(dt.repl)[, y_cname]
                if (force_y) {
                    new_col = ifelse(!is.na(y_col), y_col, x_col)
                } else {
                    new_col = ifelse(is.na(x_col) & !is.na(y_col), y_col, x_col)
                }                
                dt.repl[, eval(dc(c(x_cname, y_cname))) := NULL]
                dt.repl[, eval(dc(this_col)) := new_col]
            })
            ## dt.x.tmp = copy(dt.x)[, eval(dc(these_cols)) := NULL]
            ## lapply(these_cols, function(this_col)
            ## {
            ##     eval_x = paste0("x = ", this_col)
            ##     ## tmp_x = copy(dt.x)[, eval(dl(eval_x))]
            ##     eval_y = paste0("y = ", this_col)
            ##     ## tmp_y = copy(dt.y)[, eval(dl(eval_y))]
            ##     tmp = cbind(copy(dt.x)[, eval(dl(eval_x))],
            ##                 copy(dt.y)[, eval(dl(eval_y))])
            ##     new_val = tmp[, ifelse(is.na(x) & !is.na(y),
            ##                  y,
            ##                  x)]
            ##     dt.x.tmp[, eval(dc(this_col)) := new_val]
            ## })
        }
    } else if (!replace_in_x & !is.null(suffix)) {
        y.suff.cols = paste0(y.cols, sep, suffix)
        dt.y.tmp = copy(dt.y)[, eval(dc(y.suff.cols)) := eval(dl(y.cols))][, eval(dc(y.cols)) := NULL]
        dt.repl = merge(dt.x, dt.y.tmp, all.x = TRUE, ...)
    }
    return(dt.repl)
}

merge.suff = function(dt.x, dt.y, suffix = NULL, replace_in_x = FALSE,  sep = "_") {
    return(merge.repl(dt.x, dt.y, replace_in_x = replace_in_x, suffix = suffix, sep = sep))
}

lg2int = function(dt) {
    these_cols = which(sapply(dt, class) == "logical")
    for (this_col in these_cols) {
        this_val = as.data.frame(dt[, this_col, with = FALSE])[,1]
        data.table::set(dt, j = this_col, value = as.integer(this_val))
    }
    return(dt)
}
        
dt_na2false = function(dt) {
    these_cols = which(sapply(dt, class) == "logical")
    for (this_col in these_cols) {
        this_val = as.data.frame(dt[, this_col, with = FALSE])[,1]
        data.table::set(dt, j = this_col, value = na2false(this_val))
    }
    return(dt)
}

dt_na2zero = function(dt) {
    these_cols = which(sapply(dt, class) %in% c("numeric", "integer"))
    if (!inherits(dt, "data.table")) {
        setDT(dt)
    }
    for (this_col in these_cols) {
        this_val = as.data.frame(dt)[, this_col]
        this_val[is.na(this_val)] = 0
        data.table::set(dt, j = this_col, value = this_val)
        ## dt[, this_col] = this_val
    }
    return(dt)
}



dt_na2empty = function(dt) {
    these_cols = which(sapply(dt, class) == "character")
    for (this_col in these_cols) {
        this_val = as.data.frame(dt[, this_col, with = FALSE])[,1]
        data.table::set(dt, j = this_col, value = na2empty(this_val))
    }
    return(dt)
}

dt_setnull = function(dt, cols) {
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = NULL)
    }
    return(dt)
}


dt_setint = function(dt, cols = NULL) {
    if (is.null(cols)) {
        cols = names(dt)[which(sapply(dt, class) %in% c("numeric"))]
    }
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = as.integer(dt[[this_col]]))
    }
    return(dt)
}

dt_setchar = function(dt, cols = NULL) {
    if (is.null(cols)) {
        cols = names(dt)[which(!sapply(dt, class) == "character")]
    }
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = as.character(dt[[this_col]]))
    }
    return(dt)
}
