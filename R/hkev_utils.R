vplot2 = function(dat,
                  y_field,
                  group_field,
                  facet1 = NULL,
                  facet2 = NULL,
                  transpose = FALSE,
                  scale = "width",
                  stat = "ydensity",
                  position = "dodge",
                  xlab = NULL,
                  ylab = NULL,
                  cex.scatter = 2,
                  alpha = 0.5,
                  trim = TRUE,
                  fill_by = NULL,
                  title = NULL)
{
    ## browser()
    dat2 = setDT(copy(dat))
    y_field = substitute(y_field)
    group_field = substitute(group_field)
    facet1 = substitute(facet1)
    facet2 = substitute(facet2)
    facet1 = dat2[, eval(facet1)]
    facet2 = dat2[, eval(facet2)]
    if (!is.null(facet1))
        if (!is.factor(facet1))
            facet1 = factor(facet1, unique(facet1))
    if (!is.null(facet2))
        if (!is.factor(facet2))
            facet2 = factor(facet2, unique(facet2))
    dat2[, y := eval(y_field)]
    group = dat2[, eval(group_field)]
    fill_by = substitute(fill_by)
    if (is.null(fill_by)) {
        fill_by = group
    } else {
        fill_by = dat2[, eval(fill_by)]
    }

    if (!is.factor(group))
        group = as.factor(group)
    suppressWarnings(dat2[, `:=`(facet1, facet1)])
    suppressWarnings(dat2[, `:=`(facet2, facet2)])
    dat2 = dat2[rowSums(is.na(dat2)) == 0, ]
    dat2$vgroup = paste(dat2$group, dat2$facet1, dat2$facet2)
    vgroup = NULL
    vfilter = TRUE
    good = as.data.table(dat2)[, list(var = var(y)), keyby = vgroup][var >
                                                                    0, vgroup]
    dat2 = dat2[, `:=`(vfilter, dat2$vgroup %in% as.character(good))]
    v = ggplot(data = dat2, aes(y = y, x = group)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank(), axis.line = element_line(colour = "black"))
    v = v + geom_violin(mapping = aes(fill = fill_by), stat = stat, position = position,
                        trim = trim, scale = scale)
    v = v + geom_jitter(mapping = aes(fill = fill_by), shape = 21, size = cex.scatter, alpha = alpha, position = position_jitter(height = 0))
    if (!is.null(ylab)) v = v + ylab(ylab)
    if (!is.null(title)) v = v + ggtitle(title)
    if (!is.null(dat2$facet1)) {
        if (!is.null(dat2$facet2)) {
            if (transpose)
                v = v + facet_grid(facet2 ~ facet1)
            else v = v + facet_grid(facet1 ~ facet2)
        }
        else {
            if (transpose)
                v = v + facet_grid(. ~ facet1)
            else v = v + facet_grid(facet1 ~ .)
        }
    }
    return(v)
}


vplot = function(y, group = 'x', facet1 = NULL, facet2 = NULL, transpose = FALSE, flip = FALSE,  mapping = NULL,
                 stat = "ydensity",
                 position = "dodge",
                 trim = TRUE, sample = NA, scale = "width", log = FALSE, count = TRUE, xlab = NULL, ylim = NULL, ylab = NULL, minsup = NA,
                 scatter = FALSE,
                 text = NULL,
                 reorder = FALSE,
                 reorder.fun = mean,
                 cex.scatter = 1,
                 col.scatter = NULL, alpha = 0.3, title = NULL, legend.ncol = NULL, legend.nrow = NULL, vfilter = TRUE, vplot = TRUE, dot = FALSE, stackratio = 1, binwidth = 0.1, plotly = FALSE, print = TRUE,
                 base_size = 11,
                 blank_theme = TRUE,
                 col = NULL,
                 flip_x = TRUE,
                 drop = FALSE,
                 facet_scales = c("fixed", "free_y", "free_x", "free"))
{
                                        # require(ggplot2)
    if (!is.factor(group))
        group = as.factor(group)
    dat = data.table(y = suppressWarnings(as.numeric(y)), group)

    if (reorder)
    {
        newlev = dat[, reorder.fun(y, na.rm = TRUE), by = group][order(V1), group]
        dat[, group := factor(group, levels = newlev)]
    }

    if (!is.na(sample))
        if (sample>0)
        {
            if (sample<1)
                dat = dat[sample(nrow(dat), round(sample*nrow(sample))), ]
            else
                dat = dat[sample(nrow(dat), round(sample)), ]
        }

    if (is.null(facet1))
    {
        facet1 = facet2
        facet2 = NULL
    }

    if (!is.null(facet1))
        if (!is.factor(facet1))
            facet1 = factor(facet1, unique(facet1))


    if (!is.null(facet2))
        if (!is.factor(facet2))
            facet2 = factor(facet2, unique(facet2))

    suppressWarnings(dat[, facet1 := facet1])
    suppressWarnings(dat[, facet2 := facet2])

    dat = dat[rowSums(is.na(dat))==0, ]

    ## remove 0 variance groups
    dat$vgroup = paste(dat$group, dat$facet1, dat$facet2)

    ## if (vfilter)
    ##     {
    vgroup = NULL ## NOTE fix
    good = as.data.table(dat)[, list(var = var(y)), keyby = vgroup][var>0, vgroup]
    dat = dat[, vfilter := dat$vgroup %in% as.character(good)]
    ## }

    if (!is.na(minsup))
    {
        num = NULL ## NOTE fix
        good = as.data.table(dat)[, list(num = length(y)), keyby = vgroup][num>minsup, vgroup]
        dat = dat[(dat$vgroup %in% as.character(good)), ]
    }

    if (nrow(dat)==0)
        stop('No groups exist with >0 variance')

    if (count)
    {
        tmp = table(dat$group)
        ix = match(levels(dat$group), names(tmp))
        levels(dat$group) = paste(names(tmp)[ix], '\n(', tmp[ix], ')', sep = '')
    }

    if (is.null(mapping))
        mapping = aes(fill=group)

    if (!is.null(col)) {
        dat[, col := col]
    }
    ## g = ggplot(dat[vfilter!=0, ], aes(y = y, x = group, group = group))
    g = ggplot(dat, aes(y = y, x = group, group = group))
    g = g + theme_bw(base_size = base_size)
    if (blank_theme) {
        if (flip_x) {
            g = g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x  = element_text(angle = 90, vjust = .5))
        } else {
            g = g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank(), axis.line = element_line(colour = "black"))
        }
    }

    if (vplot)
        g = g + geom_violin(mapping = mapping, stat = stat, position = position, trim = trim, scale = scale)

    if (scatter) {
        if (dot)
        {
            if (is.null(text))
                g = g + geom_dotplot(data = dat, mapping = aes(x = group, y = y, fill = group), binaxis = 'y', position = 'identity', col = NA, alpha = alpha, method = 'dotdensity', dotsize = cex.scatter, stackratio = stackratio, binwidth = binwidth, stackdir = 'center')
            else
                g = g + geom_dotplot(data = dat, mapping = aes(x = group, y = y, fill = group, text = text), binaxis = 'y', position = 'identity', col = NA, alpha = alpha, method = 'dotdensity', dotsize = cex.scatter, stackratio = stackratio, binwidth = binwidth, stackdir = 'center')
        }
        else
        {
            if (is.null(text))
            {
                if (is.null(col.scatter)) {
                    g = g + geom_jitter(data = dat, mapping = aes(fill = group), shape = 21, size = cex.scatter, alpha = alpha, position = position_jitter(height = 0))
                    ## g = g + geom_jitter(data = dat, shape = 21, size = cex.scatter, alpha = alpha, position = position_jitter(height = 0))
                }
                else
                    g = g + geom_jitter(data = dat, fill = alpha(col.scatter, alpha), shape = 21, position = position_jitter(height = 0))

            }
            else
            {
                if (is.null(col.scatter)) {
                    g = g + geom_jitter(data = dat, mapping = aes(fill = group, text = text), shape = 21, size = cex.scatter, alpha = alpha, position = position_jitter(height = 0))
                }
                else
                    g = g + geom_jitter(data = dat, mapping = aes(text = text), fill = alpha(col.scatter, alpha), shape = 21, position = position_jitter(height = 0))
            }
        }

    }



    if (log)
    {
        if (!is.null(ylim))
            if (length(ylim)!=2)
                ylim = NULL

        if (is.null(ylim))
            g = g + scale_y_log10()
                                        #                    g = g + coord_trans(y = 'log10')
        else
            g = g+ scale_y_log10(limits = ylim)
                                        #                    g = g + coord_trans(y = 'log10', limits = ylim)
    }
    else
    {
        if (!is.null(ylim))
            if (length(ylim)==1)
                g = g+ ylim(ylim[1])
            else if (length(ylim)==2)
                g = g+ ylim(ylim[1], ylim[2])
    }

    if (!is.null(xlab))
        g = g+ xlab(xlab)

    if (!is.null(ylab))
        g = g+ ylab(ylab)

    if (!is.null(title))
        g = g + ggtitle(title)

    if (!is.null(legend.ncol))
        g = g + guides(fill = guide_legend(ncol = legend.ncol, byrow = TRUE))

    if (!is.null(legend.nrow))
        g = g + guides(fill = guide_legend(nrow = legend.nrow, byrow = TRUE))


    if (flip)
        g = g + coord_flip()

    if (!is.null(dat$facet1)) {
        if (length(facet_scales) > 1) {
            message("length > 1 arg provided to facet_scales\ndefaulting to facet_scales[1]")
            facet_scales = facet_scales[facet_scales %in% c("fixed", "free_y", "free_x", "free")][1]
            if (length(facet_scales) == 0) {
                stop("facet_scales argument incorrect, must be one of\n", c("fixed", "free_y", "free_x", "free"))
            }
        }
        if (!is.null(dat$facet2)) {
            if (transpose) {
                g = g + facet_grid(facet2 ~ facet1, drop = drop, scales = facet_scales)
            } else {
                g = g + facet_grid(facet1 ~ facet2, drop = drop, scales = facet_scales)
            }
        } else {
            if (transpose) {
                g = g + facet_grid(. ~ facet1, drop = drop, scales = facet_scales)
            } else {
                g = g + facet_grid(facet1 ~ ., drop = drop, scales = facet_scales)
            }
        }
    }


    if (plotly)
        return(ggplotly(g))

    if (print)
        print(g)
    else
        g
}



#' match x indices in terms of y
#'
#' @param x A vector
#' @param y A vector
#' @return a vector of indices of \code(x) ordered by \code(y)
#' @examples
#' match_s(c(1,3,5,7,9), c(9, 5, 3))
#' match_s(c(1,3,5,7,9), c(3, 5, 9))
match_s = function(x, y) {
    ## x_tmp = factor(as.character(x), levels = as.character(y))
    ## y_tmp = factor(as.character(y), levels = as.character(x))
    ## y_tmp[which(y_tmp %in% x_tmp)]
    x_tmp = setNames(as.character(x), as.character(x))
    x_ind = setNames(1:length(x), as.character(x))
    y_tmp = setNames(as.character(y), as.character(y))
    y_ind = setNames(1:length(y), as.character(y))
    ## return(x_ind[names(y_tmp)[which(y_tmp %in% x_tmp)]])
    these_idx = which(y_tmp %in% x_tmp)
    find_in_x = names(y_tmp)[these_idx]
    names(find_in_x) = y_ind[these_idx]
    return(setNames(x_ind[find_in_x], names(find_in_x)))
}

#' matches x in terms of y
#'
#' returns vector of indices of matches in x with length of vector = length(y)
#' non matches are NA
match2 = function(x, y) {
    ## x_tmp = factor(as.character(x), levels = as.character(y))
    ## y_tmp = factor(as.character(y), levels = as.character(x))
    ## y_tmp[which(y_tmp %in% x_tmp)]
    x_tmp = setNames(as.character(x), as.character(x))
    x_ind = setNames(1:length(x), as.character(x))
    y_tmp = setNames(as.character(y), as.character(y))
    y_ind = setNames(1:length(y), as.character(y))
    ## return(x_ind[names(y_tmp)[which(y_tmp %in% x_tmp)]])
    these_idx = which(y_tmp %in% x_tmp)
    find_in_x = names(y_tmp)[these_idx]
    names(find_in_x) = y_ind[these_idx]
    new_index = rep(NA, length(y_tmp))
    new_index[y_ind[these_idx]] = x_ind[find_in_x]
    return(new_index)
}


brew = function (x, palette = "Accent")
{
    if (!is.factor(x))
        x = factor(x)
    ucols = structure(brewer.master(length(levels(x)), palette = palette), names = levels(x))
    return(ucols[x])
}


#' find all duplicates in a vector
#'
#' @param vec A vector
#' @return a logical vector with all positions marked TRUE being duplicates
#' @examples
#' find_dups(c(1,1,1,3,5))
#' find_dups(c(1,3,1,3,1))
#' find_dups(c(3,1,5,4,4))
find_dups = function(vec) {
    dups = unique(vec[ duplicated(vec)])
    vec %in% dups
}


#' @name undup
#' @title an alternative to base::unique() that preserves names
#'
#' @param obj an R vector
#' @return unique values of obj with names preserved
undup = function(obj, fromLast = FALSE, nmax = NA) {
    obj[!duplicated(obj, fromLast = fromLast, nmax = NA)]
}

selfname = function(char) {setNames(char, char)}

#' @title check_lst
#' checking a list for any elements that are try-errors
#' usually from an lapply(..., function(x) try({})) call
#'
#' @param lst A list
#' @return a logical vector marking which elements are try-errors"
check_lst = function(lst)
{
    sapply(lst, function(x) class(x)[1]) == "try-error"
}

#' a wrapper around check_lst
#'
#' @param lst A list (usually the output of lapply(... , function(x) try({}))
#' @return only returns the non-errors in the list
ret_no_err = function(lst)
{
    return(lst[!check_lst(lst)])
}

#' a wrapper around check_lst
#'
#' @param lst A list (usually the output of lapply(... , function(x) try({}))
#' @return only returns the errors in the list
ret_err = function(lst)
{
    return(lst[check_lst(lst)])
}

#' using check_lst to return
#'
#' @param lst A list (usually the output of lapply(... , function(x) try({}))
#' @return returns full length list with errored elements changed to NA
ret_na_err = function(lst)
{
    lst[check_lst(lst)] = NA
    return(lst)
}


ret_ind = function(x, ix = 1)
{
    if (class(x)[1] == "try-error")
    {
        return(x)
    } else
    {
        x[[ix]]
    }
}

#' convenience function to set column names
#'
#' @param object tabled object
#' @param nm names of the new columns
#' @return colnamed object
setColnames = function(object = nm, nm = NULL, pattern = NULL, replacement = "") {
    if (!is.null(nm)) {
        colnames(object)  = nm
    } else if (!is.null(pattern)) {
        colnames(object) = gsub(pattern, replacement, colnames(object))
    }
    return(object)
}

#' convenience function to set row names
#'
#' @param object tabled object
#' @param nm names of the new columns
#' @return rownamed object
setRownames = function(object = nm, nm) {
    rownames(object) = nm
    object
}

#' a convenience function that unlists the output of lapply
#'
#' @param ... An expression call to lapply
#' @return An unlisted output of a lapply call
unlapply = function(...) {
    return(unlist(lapply(...)))
}


#' label the quantiles in a numeric vector
#'
#' @param x A vector
#' @param g number of quantiles to bin the vector x
#' @return a factor labeling the quantile for each value in x
#' @examples
#' label_quantiles(1:100, 4)
#' label_quantiles(1:100, 10)
label_quantiles = function(x, g = 4) {
    is.hmisc = library(Hmisc, logical.return = TRUE)
    if (! is.hmisc) {
        stop("this function requires Hmisc")
    }
    quant = as.integer(cut2(x, g = g))
    lev = levels(cut(0:100, quantile(0:100, prob = 0:g/g), include.lowest = T))
    labels = factor(lev[quant], levels = lev)
    return(labels)
}



#' find out if the list is nested
#'
#' @param x A list
#' @return a logical vector indicating if the list is nested
isNested <- function(x) {
    if (class(x) != "list") {
        stop("Expecting 'x' to be a list")
    }
    out <- any(sapply(x, is.list))
    return(out)
}

#' collate two vectors together
#'
#' @param ... A set of vectors to collate
#' @return a vector with values of inputs collated together
#' @examples
#' intercalate(c("a","d","f"), c("b", "e", "g", "z"))
#' @export
intercalate = function(...) {
    args = list(...)
    if (isNested(args)) {
        args = unlist(args, recursive = F)
    }
    s_along = lapply(args, seq_along)
    ord = order(do.call(c, s_along))
    conc = do.call(c, args)
    return(conc[ord])
}


#' convenience function to convert to matrix
#' and optionally filter out the first column
#' which may be rownames that are not relevant to further data analysis
#'
#' @param obj a data.frame or matrix
#' @param rm_col1 a logical vector specifying if the 1st column should be removed
#' @return a matrix
#' @export
matrify = function(obj, rm_col1 = TRUE) {
    if (rm_col1) {
        as.matrix(obj[,-1])
    } else {
        as.matrix(obj)
    }
}


#' collate lists together
#'
#' @param ... A set of lists to collate
#' @return a lists with elements collated together
#' @examples
#' intercalate(list(paste0(1:5, "_A")), list(paste0(1:3, "_B")), list(paste0(1:6, "_C")))
intercalate_lst = function(...) {
    args = list(...)
    s_along = lapply(args, seq_along)
    ord = order(do.call(c, s_along))
    conc = do.call(c, args)
    return(conc[ord])
}


#' utility function for removing multiple parantheses
#' probably not necessary
#'
#' @param str a path string
#' @return a string with multiple parentheses replaced with a single parenthesis
rm_mparen  = function(str) {
    return(gsub('\\/{2,}', "/", str))
}


#' qstat parsing
#'
#' @param query a string of additional switches provided to qstat
#' @return NULL
make_qstat_query = function(query = "", tmpdir = '~/tmp/qstat_query/') {
    tmpfn = gsub('\\/{2,}', "/", paste0(tmpdir, "/q.xml"))
    system(paste0("mkdir -p ", tmpdir))
    cmd = paste0("qstat -xml ", query, " > ", tmpfn)
    system(cmd)
}


#' qstat query parsing
#'
#' @param query String that is either empty or -u <username>
#' @param tmpdir temporary directory path where the qstat info is dumped for reading into R
#'
#' @return a data table containing the full queue information at time of calling this function
qstat_query = function(query = "", tmpdir = "~/tmp/qstat_query/") {
    library(XML)
    make_qstat_query(query = query, tmpdir = tmpdir)
    tmpfn = gsub('\\/{2,}', "/", paste0(tmpdir, "/q.xml"))
    this_xml = xmlToList(tmpfn, simplify = F)
    qinfo = rbindlist(lapply(this_xml$queue_info, function(x) as.data.table(t(x))))

    if (! nchar(trimws(unlist(as.data.frame(qinfo[1,1])))) == 0) {
        qinfo[, jclass_name := sapply(jclass_name, function(x) switch(is.null(NULL), NULL = NA, x))][, queue_name := sapply(queue_name, function(x) switch(is.null(NULL), NULL = NA, x))]
    } else {
        qinfo = NULL
    }

    jinfo = rbindlist(lapply(this_xml$job_info, function(x) as.data.table(t(x))))

    if ( ! nchar(trimws(unlist(as.data.frame(jinfo[1,1])))) == 0) {
        jinfo[, jclass_name := sapply(jclass_name, function(x) switch(is.null(NULL), NULL = NA, x))][, queue_name := sapply(queue_name, function(x) switch(is.null(NULL), NULL = NA, x))]
    } else {
        jinfo = NULL
    }

    fullinfo = rrbind(qinfo, jinfo, as.data.table = TRUE)
    fullinfo = as.data.table(lapply(fullinfo, unlist))
    system(paste0('rm ', tmpfn))
    return(fullinfo)
}



#' qstat job id querying
#' INCOMPLETE
#'
#' @param jid job id numbers, usually gotten from qstat_query
#' @param tmpdir temporary directory path where the qstat info is dumped for reading into R
#'
#' @return a data table containing job information
qstat_jquery = function(jid, tmpdir = "~/tmp/qstat_query/") {
    tmpfn = gsub('\\/{2,}', "/", paste0(tmpdir, "/q.xml"))
    these_j = rbindlist(lapply(jid, function(j) {
        make_qstat_query((paste0("-j ",  j)))
        this_xml = xmlToList(tmpfn)
        this_j = rbindlist(lapply(this_xml$djob_info, function(x) as.data.table(t(x))))[, JB_preemption := sapply(JB_preemption, function(x) ifelse(is.null(x), NA, x))]
        return(this_j)
    }))
    ## browser()
    ## these_j = as.data.table(lapply(these_j, function(x) unlist(x, recursive = FALSE)))
    return(these_j)
}

#' @name ix_sdiff
#'
#' A function that subsets out indices and is robust to
#' if filt_out indices are integer(0)
#'
#' @return obj with indices indicated in filt_out taken out
ix_sdiff = function(obj, filt_out) {
    if (is.null(nrow(obj))) {
        ix = 1:length(obj)
    } else {
        ix = 1:nrow(obj)
    }
    obj[! ix %in% filt_out]
}



#' parse the output of gGnome::fusion()
#'
#' Take the fusion outputs and separate into 5' and 3' partners
#'
#' @param fus GRangeslist object output of gGnome::fusion()
#' @return data table containing breakpoints and the name of fusion partners
parse_pair_fus = function(fus, only_unique = TRUE)
{
    tmp_dt = gr2dt(grl.unlist(fus))
    star_formatted = tmp_dt[,
    {
        this_iix = .SD[, grl.iix][-.N]
        ids_by_2 = intercalate(c(this_iix, this_iix + 1))
        names(ids_by_2) = rep(1:(length(ids_by_2)/2), each = 2)
        dt = copy(.SD)[ids_by_2]
        dt[, split_by := names(ids_by_2)]
        dt2 = dt[,{
            left = copy(.SD)[1]
            right = copy(.SD)[2]
            #' be careful here
            #' it seems like the star breakpoints are specified
            #' as reference coordinate junctions (the fused sides)
            #' the fusions are in walk format...
            #' so need to specify the actual fused breakpoints
            left_bp = ifelse(left$strand == "+", left$end, left$start-1)
            left_strand = ifelse(left$strand == "+", "-", "+")
            right_bp = ifelse(right$strand == "+", right$start-1, right$end)
            right_strand = ifelse(right$strand == "+", "+", "-")
            list(Left_Gene = left$gene_name,
                 Right_Gene = right$gene_name,
                 Left_Breakpoint = paste0(left$seqnames, ":", left_bp, ":", left_strand),
                 Right_Breakpoint = paste0(right$seqnames, ":", right_bp, ":", right_strand),
                 Left_InFrame = left$in.frame,
                 Right_InFrame = right$in.frame,
                 Fusion = left$alteration)
        }, by = split_by]
        dt2[, Fusion_Name := paste0(Left_Gene, "--", Right_Gene)]
        dt2
        ## bla = matrix(intercalate(c(this_iix, this_iix + 1)), nrow = length(this_iix), ncol = 2, dimnames = list(NULL, c("left_idx", "right_idx")))
    }, by = grl.ix]
    if (only_unique) {
        star_formatted = star_formatted[!duplicated(Fusion_Name)]
    }
    return(star_formatted)
}

#' splitting a uniform string
#'
#' Splits a uniform string (i.e. a strings that when split, give a list of all equal sized elements
#'
#' @param str string to split
#' @param split regex to split by
#' @return a matrix of the strings
str_mat = function(str, split = "\\:")
{
    tmp_mat = do.call(rbind, strsplit(str, split = split))
}


#' making na's false
#'
na2false = function(v)
{
    ## v = ifelse(is.na(v), v, FALSE)
    v[is.na(v)] = FALSE
    as.logical(v)
}

#' making na's empty characters
na2empty = function(v) {
    ## v = ifelse(is.na(v), v, FALSE)
    v[is.na(v)] = ""
    as.character(v)
}

#' make data.frame or data.table column name whitespaces into underscores and remove end whitespaces
#'
ws2und = function(df)
{
    data.table::setnames(df, gsub("^_|_$", "", gsub("_{2,}", "_", gsub("(\\.)|( )|\\(|\\)|\\#", "_", trimws(colnames(df))))))
    return(df)
}


#' test if all elements of object are the same
#'
#' This may not work with numeric
#'
allsame = function(obj)
{
    all(obj == obj[1])
}


#' filter sv by overlaps with another
#'
#' To filter out a grangeslist of sv by another grangeslist of SV
#'
#' @param sv GRangesList with all elements length 2 (specifying breakpoint pairs of a junction)
#' @param filt_sv GRangesList with all elements length 2 (usually a pon)
#' @param pad Exposed argument to skitools::ra.overlaps()
#' @return GRangesList of breakpoint pairs with junctions that overlap removed
sv_filter = function(sv, filt_sv, pad = 500)
{
    within_filt = skitools::ra.overlaps(sv, filt_sv, pad = pad)
    ## filter_these = unique(within_tcga_germ[,"ra1.ix"])
    filter_these = unique(within_filt[,1])
    sv = ix_sdiff(sv, filter_these)
    return(sv)
}


#' assign an object to global environment
#'
#' ONLY USE IF YOU KNOW WHAT YOU ARE DOING
#' This function forces assignment of a variable/function
#' to the global environment
#'
#' @param obj The object to assign to the global environment
#' @param var Optional name of variable, specified as string
#' @return NULL Assigns the value in obj to the global environment
globasn = function(obj, var = NULL)
{
    if (is.null(var)) {
        globx = as.character(substitute(obj))
    } else {
        if (!is.character(var)) {
            stop("var must be specified as character")
        }
        globx = as.character(substitute(var))
    }
    assign(globx, value = obj, envir = .GlobalEnv)
}

#' wrapper around sum(as.numeric())
#'
#' since sum() sometimes fails when you have non floating point values
#' (e.g. integers), this function wrapping around sum(as.numeric()) will prevent_this
#'
#' @param vec A vector of values
#' @return Sum of this vector
n_sum = function(vec)
{
    sum(as.numeric(vec))
}

#' wrapper around as.data.frame
#'
#' convenience function
as.df = function(obj) {
    return(as.data.frame(obj))
}

## #' defining operators
## #'
## `%Q%` <<- gUtils::`%Q%`

## #' using operators (outside of a %Q% b context)
## gUtils::`%Q%`(a, b)
## `%Q%`(a, b)


#' adding column to grangeslist elements
grl.val = function(grl, column, val) {
    g.unlist = grl.unlist(grl)
    values(g.unlist)[, column] = val
    return(relist(g.unlist, "grl.ix"))
}


#' updating values of granges/grangeslist
#'
#' convenience function
#'
#' @param g A GRanges or GRangesList object
#' @param col A column of GRanges
#' @param val Values to place in column
#' @return GRanges/GRangesList object with updated values
#' @export
newval = function(g, col, val) {
    if (length(col) > 1) {
        is_list = is.list(val)
        if (!is_list) {
            if ((length(col) != length(val) & length(val) > 1)) {
                stop("val must be list with length(val) == length(col) when assigning multiple columns, with different values")
            }
        }
        if (length(val) == 1 | is.null(val)) {
            for (i in 1:length(col)) {
                mcols(g)[, col[i]] = val[[1]]
            }
        } else if (length(val) > 1) {
            if (is_list) {
                for (i in 1:length(col)) {
                    mcols(g)[, col[i]] = val[[i]]
                }
            } else {
                for (i in 1:length(col)) {
                    mcols(g)[, col[i]] = val
                }
            }
        }
    } else {
        mcols(g)[, col] = val
    }
    return(g)
}



checkfin = function(outpath) {
    cmd = paste0("tail -n 10 ", outpath, " | grep \"Exit status\\:\"")
    finish_line = system(cmd, intern = TRUE)
    if (length(finish_line) > 0) {
        return("finished")
    } else {
        return(structure("unfinished", class = "try-error"))
    }
}

flowfin = function(outpaths, mc.cores = 1) {
    these_outs = mclapply(mc.cores = mc.cores, X = outpaths, FUN = function(o) {
        checkfin(o)
    })
}


#' simply print character vector to console
#'
#' Convenience function to print a character vector to console for easy copy paste
#'
#' @param str A character vector
#' @param sep The delimiter of choice
#' @return NULL
ezcat = function(str, sep = "\n") {
    cat(str, sep = sep)
}

take_split = function(str, split = "\\.", ix = 1) {
    this_split = strsplit(str, split = split)
    unlist(lapply(this_split, function(spl) {
        spl[ix]
    }))
}


seedir = function(path, pattern = NULL, dir = FALSE) {
    if (! dir) {
        path = dirname(path)
    }
    return(dir(path, full.names = TRUE, pattern = pattern))
}


tr.fix = function(gt, y0 = 0, stack.gap = 2, height = 10, ygap = 2, y1 = NULL, name = NULL, cex.label = NULL) {
    gt$ygap = ygap
    gt$stack.gap = stack.gap
    gt$height = height
    if (!is.na(gt$y0)) {
        gt$y0 = y0
    }
    if (!is.null(name)) {
        gt$name = name
    }
    if (!is.null(y1) & !is.na(gt$y1)) {
        gt$y1 = y1
    }
    if (!is.null(cex.label)) {
        gt$cex.label = cex.label
    }
    return(gt)
}



discard_dups = function(reads.grl, as.grl = TRUE, as.dt = FALSE) {
    if (as.grl) {
        as.dt = FALSE
    } else if (as.dt) {
        as.grl = FALSE
    }
    grl.val = values(reads.grl)
    dt_grl = gr2dt(grl.unlist(reads.grl))
    dt_grl[, tmp_col := paste(qname, start, end, cigar, strand, flag)]
    ## dt_grl[, tmp_col := paste(qname, start, end, strand, flag)]
    not_dup = dt_grl[, not_dup := !duplicated(tmp_col), by = grl.ix]$not_dup ## grl.ix should track with qname
    dt_grl[, tmp_col := NULL]
    ## unique(dt_grl[not_dup]$grl.ix)
    dt_grl = dt_grl[not_dup]
    dt_grl[, not_dup := NULL]
    if (as.dt) {
        return(structure(dt_grl, grl.val = grl.val))
    }
    if (as.grl) {
        dt_grl = dt_setnull(dt_grl, colnames(grl.val))
        new.reads.grl = dt2gr(dt_grl)
        new.reads.grl = split(new.reads.grl, new.reads.grl$grl.ix)
        values(new.reads.grl) = grl.val
        return(new.reads.grl)
    }
}



#' discarding duplicates in read pairs when using interval/gr query in read.bam
#'
read.bam2 = function(bam, gr, ..., discard.dups = TRUE, query_mate = TRUE) {
    reads.grl = read.bam(bam = bam, gr = gr, pairs.grl = TRUE, ...)
    if (discard.dups) {
        dt_grl = discard_dups(reads.grl, as.dt = TRUE, as.grl = FALSE)
    } else {
        dt_grl = grl.unlist(reads.grl)
    }
    ix = dt_grl[, rep(.N == 1, .N), by = qname]$V1
    reads_missing_pair = dt_grl[ix]
    pair_query = GRanges(reads_missing_pair$mrnm, IRanges(start = reads_missing_pair$mpos, end = reads_missing_pair$mpos))
    missing_mates = discard_dups(read.bam(bam = bam, gr = pair_query))
    missing_mates = gr2dt(unlist(missing_mates))
    missing_mates = missing_mates[paste(qname, mrnm, mpos) %in% reads_missing_pair[, paste(qname, seqnames, start)]]
    tmp_add = rrbind(reads_missing_pair, missing_mates)
    all_mates = rrbind(dt_grl[!ix], tmp_add, as.data.table = TRUE)
    all_mates[, `:=`(col = NULL, border = NULL, grl.ix = NULL, grl.iix = NULL)]
    reads_w_mates = dt2gr(all_mates)
    reads_w_mates = split(reads_w_mates, reads_w_mates$qname)
    values(reads_w_mates) = data.frame(col = rep("gray", length(reads_w_mates)), border = rep("gray", length(reads_w_mates)))
    return(reads_w_mates)
}



#' altering the same field across list objects
alt_lst = function(..., expr = NULL, FUN = NULL, do.c = FALSE) {
    this_err = grepl("which requires 1", try((...), silent = TRUE))
    if (this_err) {
        obj = list(...)
    } else {
        if (is.list(...)) {
            obj = (...)
        }
    }
    if (!is.null(expr)) {
        if (is.character(expr)) {
            expr = parse(text = expr)
        }
    }
    ## obj = obj[which(!sapply(obj, is.null))]
    is_null = sapply(obj, is.null) ## try this
    if (is.null(names(obj))) {
        nm = sapply(substitute(list(...))[-1], deparse)
        names(obj) = nm
    }
    if (is.null(FUN) & !is.null(expr)) {
        obj = lapply(obj, function(.OBJ) {
            if (!is.null(.OBJ)) {
                eval(expr)
                return(.OBJ)
            } else {
                return(NULL)
            }
        })
    } else if (!is.null(FUN)) {
        tmp_obj = lapply(obj[!is_null], FUN) ## try this
        obj[!is_null] = tmp_obj
        ## obj = lapply(obj, FUN)
    }
    if (do.c) {
        do.call("c", obj)
    }
    return(obj)
}

reassign = function(variables_lst, calling_env = parent.frame()) {
    for (i in 1:length(variables_lst)) {
        assign(names(variables_lst[i]), variables_lst[[i]], envir = calling_env)
    }
}

## reassign = function(variables_lst, calling_env = parent.frame()) {
##     ## for (i in 1:length(variables_lst)) {
##     ##     if (is.null(parent.env(environment())[[names(variables_lst[i])]])) {
##     ##         rm(list = names(variables_lst[i]), envir = parent.env(environment()))
##     ##     }
##     ##     assign(names(variables_lst[i]), variables_lst[[i]], envir = parent.env(environment()))
##     ## }
##     for (i in 1:length(variables_lst)) {
##         ## if (is.null(calling_env[[names(variables_lst[i])]])) {
##         ##     rm(list = names(variables_lst[i]), envir = calling_env)
##         ## }
##         assign(names(variables_lst[i]), variables_lst[[i]], envir = calling_env)
##     }
## }


grepout = function(str, grepe) {
    grepe = paste0(grepe, collapse = "|")
    str[!grepl(grepe, str)]
}


newelmcols = function(grl, cols, val) {
    tmp_nm = names(grl)
    gr = unlist(grl, use.names = FALSE)
    gr = newval(gr, cols, val)
    grl = relist(gr, grl)
    names(grl) = tmp_nm
    return(grl)
}


color_grlreads = function(reads_grl, plus = "red", minus = "blue") {
    tmp = gr2dt(unlist(reads_grl))
    ## avoiding naming collisions of "plus" and "minus"
    this_env = environment()
    tmp[strand == "+", col := this_env$plus]
    tmp[strand == "-", col := this_env$minus]
    tmp = dt2gr(tmp)
    tmp = relist(tmp, reads_grl)
    mcols(tmp) = mcols(reads_grl)
    return(tmp)
}


split_cigar = function(reads_grl, only_M = FALSE) {
    tmp = grl.unlist(splice.cigar(reads_grl))
    tmp$cigar = paste0(width(tmp), tmp$type)
    if (only_M) {
        tmp = tmp[na2false(tmp$type == "M")]
    }

    tmp = split(tmp, tmp$qname, drop = TRUE)
    return(tmp)
}

split_conc = function(str, ind = 1, split = "_", collapse = split, include_last = FALSE) {
    unlist(lapply(strsplit(str, split = split), function(s)
    {
        this_ind = pmin(ind, length(s))
        if (!length(s) %in% this_ind) {
            if (include_last) {
                this_ind = c(this_ind, length(s))
            }
        }
        ## intersect(this_ind, 1:length(s))
        paste0(s[this_ind], collapse = collapse)
    }))
}

dfbr = function(mod) {
    coef = as.data.frame(summary(mod)$coefficients$mean)
    colnames(coef) = c("estimate", "se", "stat", "p")
    coef$ci.lower = coef$estimate - 1.96 * coef$se
    coef$ci.upper = coef$estimate + 1.96 * coef$se
    if (mod$link$mean$name %in% c("log", "logit")) {
        coef$estimate = exp(coef$estimate)
        coef$ci.upper = exp(coef$ci.upper)
        coef$ci.lower = exp(coef$ci.lower)
    }
    nm = rownames(coef)
    out = data.frame(name = nm, method = mod$method,
                     p = signif(coef$p, 3), estimate = coef$estimate,
                     ci.lower = coef$ci.lower, ci.upper = coef$ci.upper,
                     effect = paste(signif(coef$estimate, 3), " [", signif(coef$ci.lower,
                                                                           3), "-", signif(coef$ci.upper, 3), "]", sep = ""))
    out$effect = as.character(out$effect)
    out$name = as.character(out$name)
    out$method = as.character(out$method)
    rownames(out) = NULL
    return(as.data.table(out))
    return(out)
}


plot_tsne_grid = function(dat, dat_group, col = NA, param_grid = NULL, mc.cores = 1) {
    library(ggplot2)
    if (is.null(param_grid)) {
        perps = c(10,20,30,40,50)
        iters = c(1000,3000,5000,10000)
        param_grid = expand.grid(perp = perps, iter = iters)
    }
    these_plots = mclapply(1:nrow(param_grid), function(this_row)
    {
        tmp = param_grid[this_row,]
        message("perp: ", tmp$perp, "\nmax_iter: ", tmp$iter)
        tts = Rtsne(dat, perplexity = tmp$perp, check_duplicates = FALSE, max_iter = tmp$iter)
        p = data.table(x = tts$Y[,1], y = tts$Y[,2], col = col, group = dat_group, perp = tmp$perp, max_iter = tmp$iter)
        gg = ggplot(p, aes(x = x, y = y, color = group)) + geom_point(size = 2.5) + xlab("tSNE 1") + ylab("tSNE 2") + ggtitle(sprintf("perp = %s\niter= %s", tmp$perp, tmp$iter)) + theme_bw(base_size = 25) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank(), axis.line = element_line(colour = "black"))
        if (!is.na(col)) {
            gg = gg + scale_color_manual(values = col)
        }
        return(gg)
    }, mc.cores = mc.cores, mc.preschedule = FALSE)
    expr = expression(
    {
        lapply(these_plots, function(this_plot)
        {
            print(this_plot)
        })
    })
    ppdf(eval(expr))
    return(these_plots)
    ## NULL
}


## gg plotting
gg_mytheme = function(gg, base_size = 16, legend.position = "none", flip_x = TRUE, x_axis_cex = 1, y_axis_cex = 1, ylab_cex = 1, xlab_cex = 1, title_cex = 1) {
    ## gg = gg + theme_bw(base_size = base_size) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x  = element_text(angle = 90, vjust = .5), legend.position = legend.position)
    if (flip_x) {
        gg = gg + theme_bw(base_size = base_size) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x  = element_text(angle = 90, vjust = .5, size = rel(x_axis_cex)), legend.position = legend.position, axis.text.y = element_text(size = rel(y_axis_cex)), plot.title = element_text(size = rel(title_cex)), axis.title.x = element_text(size = rel(xlab_cex)), axis.title.y = element_text(size = rel(ylab_cex)))
    } else {
        gg = gg + theme_bw(base_size = base_size) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank(), axis.line = element_line(colour = "black"), legend.position = legend.position, axis.text.x = element_text(size = rel(x_axis_cex)), axis.text.y = element_text(size = rel(y_axis_cex)), plot.title = element_text(size = rel(title_cex)), axis.title.x = element_text(size = rel(xlab_cex)), axis.title.y = element_text(size = rel(ylab_cex)))
    }
    return(gg)
}

gg_axes = function(gg, ylim = NULL, xlim = NULL) {
    if (!is.null(ylim)) {
        gg = gg + ylim(ylim)
    }
    if (!is.null(xlim)) {
        gg = gg + xlim(xlim)
    }
    return(gg)
}



ggplot_tsne = function(rtsne_res = NULL, col = NA, group = NA, perp = NA, max_iter = NA, p = NULL) {
    args_lst = grab_expl_args()
    if (is.na(col)) {
        if (!is.null(p$col)) {
            if (all(!is.na(p$col)) & all(!is.na(p$group))) {
                col = p$col
                names(col) = as.character(p$group)
                col = undup(col)
            }
        }
    }
    is.col = !is.na(col)
    if (! is.null(rtsne_res)) {
        if (is.null(p)) {
            p = data.table(x = rtsne_res$Y[,1], y = rtsne_res$Y[,2], col = col, group = dat_group, perp = perp, max_iter = max_iter)
            is.col = is.col | !is.na(unique(p$col))
        }
    } else if (is.null(rtsne_res) & is.null(p)) {
        stop("please provide either rtsne_res or p")
    }
    gg = ggplot(p, aes(x = x, y = y, color = group)) + geom_point(size = 2.5) + xlab("tSNE 1") + ylab("tSNE 2") + ggtitle(sprintf("perp = %s\niter= %s", unique(p$perp), unique(p$iter))) + theme_bw(base_size = 25) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank(), axis.line = element_line(colour = "black"))
    if (all(!is.na(col))) {
        gg = gg + scale_color_manual(values = col)
    }
    return(gg)
}


ggplot_mybar = function(y, col = NA, group = NA, fill_by = NA, facet1 = NULL, facet2 = NULL, transpose = FALSE, base_size = 16, xlab = NULL, ylab = NULL, ggtitle = NULL, legend.position = "none", print = FALSE) {
    library(forcats)
    dat = data.frame(id_col = 1:length(y), y = y, group = group, col = col, fill_by = fill_by)
    if (!is.na(col)) {
        if (length(col) != length(group) | length(col) != length(unique(group))) {
            stop("col must have 1 to 1 correspondence to unique levels within group\nor must be a length(group) vector of colors")
        }
        names(col) = as.character(p$group)
        col = undup(col)
    }
    dat[, "facet1"] = facet1
    dat[, "facet2"] = facet2
    if (!is.na(fill_by)) {
        dat[, "fill_by"] = fill_by
    } else {
        dat[, "fill_by"] = dat[['group']]
    }
    gg = ggplot(dat, mapping = aes(x = group, y = y, fill = fct_explicit_na(fill_by))) + geom_bar(stat = "identity")
    if (!is.null(dat[["facet1"]]) | !is.null(dat[["facet2"]])) {
        if (is.null(dat[["facet2"]])) {
            if (!transpose) {
                gg = gg + facet_grid(facet1 ~ ., drop = FALSE)
            } else {
                gg = gg + facet_grid(. ~ facet1, drop = FALSE)
            }
        } else if (is.null(dat[["facet1"]])) {
            if (!transpose) {
                gg = gg + facet_grid(facet2 ~ ., drop = FALSE)
            } else {
                gg = gg + facet_grid(. ~ facet1, drop = FALSE)
            }
        } else {
            if (!transpose) {
                gg = gg + facet_grid(facet1 ~ facet2, drop = FALSE)
            } else {
                gg = gg + facet_grid(facet2 ~ facet1, drop = FALSE)
            }
        }
    }
    if (!is.null(xlab)) {
        gg = gg + xlab(xlab)
    }
    if (!is.null(ylab)) {
        gg = gg + ylab(ylab)
    }
    if (!is.null(ggtitle)) {
        gg = gg + ggtitle(label = ggtitle)
    }
    if (!is.na(col)) {
        gg = gg + scale_color_manual(values = col)
    }
    gg = gg_mytheme(gg, base_size = base_size, legend.position = legend.position)
    if (print) {
        return(print(gg))
    } else {
        return(gg)
    }
}



grab_expl_args = function(deparse = TRUE, calling_env = parent.frame()) {
    expr = expression(as.list(match.call(envir = parent.frame(2)))[-1])
    ## expl_args = eval(expr, envir = parent.env(environment()))
    ## expl_args = eval(expr, envir = calling_env)
    expl_args = eval(expr, envir = calling_env)
    if (deparse) {
        return(lapply(expl_args, deparse))
    } else {
        return(expl_args)
    }
}


grab_func_name = function(deparse = TRUE, calling_env = parent.frame()) {
    expr = expression(as.list(match.call(envir = parent.frame(2)))[[1]])
    ## func_name = eval(expr, envir = parent.env(environment()))
    ## func_name = eval(expr, envir = calling_env)
    func_name = eval(expr, envir = calling_env)
    if (deparse) {
        return(deparse(func_name))
    } else {
        return(func_name)
    }
}

grab_all_args = function(deparse = TRUE, calling_env = parent.frame()) {
    this_lst = as.list(args(grab_func_name(calling_env = calling_env)))
    if (deparse) {
        given_args = sapply(this_lst, deparse)
        return(given_args)
    } else {
        return(this_lst)
    }
}

no_arg = function(as.list = TRUE, calling_env = parent.frame()) {
    these_args = grab_all_args(calling_env = calling_env)
    return_this = these_args[nchar(these_args) == 0]
    if (as.list) {
        return(as.list(return_this))
    } else {
        return(return_this)
    }
}


gsub_col = function(pattern, replacement, df) {
    ## for (i in 1:ncol(df)) {
    ##     this_colname = colnames(df)[i]
    ##     if (length(pattern) > 1 & length(replacement) > 1) {
    ##         new_colname = gsub(pattern[i], replacement[i], this_colname)
    ##     } else {
    ##         new_colname = gsub(pattern, replacement, this_colname)
    ##     }
    ##     df = setnames(df, this_colname, new_colname)
    ## }
    for (i in 1:length(pattern)) {
        these_cols = colnames(df)
        new_cols = gsub(pattern[i], replacement[i], these_cols)
        if (length(new_cols) > 0) {
            colnames(df) = new_cols
            ## df = setnames(df, these_cols, new_cols)
        } else {
            df
        }
    }
    return(df)
}


grl.expand = function(grl, expand_win) {
    tmp_vals = mcols(grl)
    tmp_gr = unlist(grl)
    tmp_gr = tmp_gr + expand_win
    new_grl = relist(tmp_gr, grl)
    mcols(new_grl) = tmp_vals
    return(new_grl)
}

grl.shrink = function(grl, shrink_win) {
    tmp_vals = mcols(grl)
    tmp_gr = unlist(grl)
    tmp_gr = tmp_gr - shrink_win
    new_grl = relist(tmp_gr, grl)
    mcols(new_grl) = tmp_vals
    return(new_grl)

}

grl.start = function(grl, width = 1, force = FALSE, ignore.strand = TRUE, clip = TRUE) {
    tmp_vals = mcols(grl)
    tmp_gr = unlist(grl)
    tmp_gr = gr.start(tmp_gr, width, force, ignore.strand, clip)
    new_grl = relist(tmp_gr, grl)
    mcols(new_grl) = tmp_vals
    return(new_grl)
}

grl.end = function(grl, width = 1, force = FALSE, ignore.strand = TRUE, clip = TRUE) {
    tmp_vals = mcols(grl)
    tmp_gr = unlist(grl)
    tmp_gr = gr.end(tmp_gr, width, force, ignore.strand, clilp)
    new_grl = relist(tmp_gr, grl)
    mcols(new_grl) = tmp_vals
    return(new_grl)
}


gr.strand = function(gr, str = "*") {
    strand(gr) = str
    return(gr)
}

gr.width = function(gr, w = width(gr)) {
    width(gr) = w
    return(gr)
}

grl.width = function(grl, w = width(grl)) {
    tmp_vals = mcols(grl)
    tmp_gr = unlist(grl)
    gr.width(tmp_gr) = w
    new_grl = relist(tmp_gr, grl)
    mcols(new_grl) = tmp_vals
    return(new_grl)
}


setMethod(`+`, 'GRangesList', function(e1, e2) {
    return(grl.expand(e1, e2))
})

setMethod(`-`, 'GRangesList', function(e1, e2) {
    return(grl.shrink(e1, e2))
})


###############################################################
grep_order = function(patterns, text, return_na = FALSE, first_only = FALSE) {
    text_ix = 1:length(text)
    match_lst = lapply(1:length(patterns), function(i) {
        these_matches = regexpr(patterns[i], text)
        position = which(these_matches != -1)
        if (first_only) {
            position = position[1]
        }
        if (length(position) > 0) {
            return(text_ix[position])
        } else if (return_na) {
            return(NA)
        }
    })
    return(unlist(match_lst))
}

grep_col_sort = function(patterns, df, all_cols = TRUE) {
    is.data.table = FALSE
    if (inherits(df, "data.table")) {
        df = as.data.frame(df)
        is.data.table = TRUE
    }
    new_col_order = grep_order(patterns, text = colnames(df), return_na = FALSE, first_only = FALSE)
    if (all_cols) {
        other_cols = setdiff(1:ncol(df), new_col_order)
        col_ix = c(other_cols, new_col_order)
    } else {
        col_ix = new_col_order
    }
    df = df[, col_ix]
    if (is.data.table) {
        return(as.data.table(df))
    } else {
        return(df)
    }
}


process_text = function(path, num_lines = 10000, extra_row = FALSE, row.names = NULL, expr = NULL, write.path = NULL) {
    con = file(description = path, open = "r")
    raw_header = readLines(con = con, n = 1)
    the_header = strsplit(raw_header, "\\s+")[[1]]
    if (extra_row) {
        the_header = c("", the_header)
    }
    if (! is.null(expr)) {
        if (inherits(expr, "character")) {
            expr = parse(text = expr)
        } else if (inherits(expr, "expression")) {
            expr
        } else {
            stop("expr either needs to be a character or expression to be evaluated within j of data_table[i, j]")
        }
    }
    if (!is.null(write.path)) {
        write.path = rm_mparen(paste0(normalizePath(dirname(write.path)), "/", basename(write.path)))
        ## write_con = file(write.path, open = "w+")
        if (file.exists(write.path)) {
            msg = sprintf("write.path=%s exists! Please remove before processing text", write.path)
            stop(msg)
        }
    }
    counter = 1
    while (TRUE) {
        these_lines = readLines(con = con, n = num_lines)
        if (length(these_lines) == 0) {
            break
        }
        tmp_tbl = read.table(text = these_lines, row.names = row.names)
        these_rownames = rownames(tmp_tbl)
        data.table::setnames(tmp_tbl, the_header)
        tmp_tbl = as.data.table(tmp_tbl)
        if (!is.null(expr)) {
            tmp_tbl[, eval(expr)]
        }
        if (!is.null(write.path)) {
            write.table(setRownames(as.data.frame(tmp_tbl), these_rownames),
                        file = write.path,
                        append = TRUE,
                        row.names = if (is.null(row.names)) { FALSE } else { as.logical(sign(row.names)) },
                        col.names = if (counter == 1) { TRUE } else { FALSE },
                        quote = FALSE,
                        sep = "\t")
        }
        message(counter * num_lines, " lines processed")
        counter = counter + 1
    }
}

scale_tracks = function(gt_obj, fields = c("height", "ygap"), scaling) {
    if (length(scaling) == 1) {
        scaling = rep(scaling, length(fields))
    }
    for (i in 1:length(fields)) {
        formatting(gt_obj)[, fields[i]] = formatting(gt_obj)[, fields[i]] * scaling[i]
    }
    return(gt_obj)
}

evens = function(vec) {
    index = 1:length(vec)
    vec[index[index %% 2 == 0]]
}

odds = function(vec) {
    index = 1:length(vec)
    vec[index[index %% 2 == 1]]
}

junc_kataegis = function(junc) {
    if (inherits(junc, "GRangesList")) {
        junc = grl.unlist(junc)
    }
    these_bp = gr2dt(sort(sortSeqlevels(junc), ignore.strand = TRUE))
    bp_dist = these_bp[,{
        .SD
        odd_start = copy(.SD)[odds(1:.N), start]
        odd_dist = c(odd_start[-1]) - c(odd_start[-length(odd_start)])
        even_start = copy(.SD)[evens(1:.N), start]
        if (.N %% 2 == 0) {
            even_dist = c(odd_start[-1], NA) - c(even_start[-length(even_start)], NA)
            return_this = c(intercalate(odd_dist, even_dist), NA)
        } else if (.N %% 2 == 1) {
            even_dist = c(odd_start[-1]) - c(even_start)
            return_this = c(intercalate(odd_dist, even_dist), NA)
        }
        ## print(return_this); print(length(return_this) == .N)
        return_this
    }, by = seqnames]
    these_bp[, bp_dist := bp_dist$V1]
    these_bp[, log_10_bdist := log10(bp_dist)]
    gr_bp = dt2gr(these_bp)
    return(gr_bp)
}




simple_hread = function(bampath) {
    library(naturalsort)
    cmd = sprintf('samtools view -H %s', bampath)
    header = system(cmd, intern = TRUE)
    this_header = read.table(text = header[2])
    tmp = lapply(this_header, function(x) x[grep("^UR|^AS", x)])
    tmp = setNames(sort(unlist(tmp)), NULL)
    if (length(tmp) == 0) {
        this_header = read.table(text = tail(header, 1))
    } else {
        return(tmp)
    }
    tmp = lapply(this_header, function(x) x[grep("LB", x)])
    tmp = setNames(sort(unlist(tmp)), NULL)
    ## tmp = as.data.frame(t(tmp))
    return(tmp)
}


hmean = function(vec) {
    return(1/(sum(1/vec)/length(vec)))
}

gmean = function(vec) {
    return(prod(vec)^(1/length(vec)))
}


seevar = function(calling_env = parent.frame()) {
    setdiff(ls(envir = calling_env), lsf.str(envir = calling_env))
}


seq_row = function(dat) {
    seq(nrow(dat))
}

seq_col = function(dat) {
    seq(ncol(dat))
}





#### dendrogram parsing

tree_stats = function(dendro) {
    tmp.env2345089712349876 = new.env(parent = globalenv())
    tmp.env2345089712349876$lst = list()
    tmp.env2345089712349876$lst2 = list()
    tmp.env2345089712349876$i = 1
    new_dend = dendrapply(dendro, function(n) {
        ht = attr(n, "height")
        mem = attr(n, "members")
        is_leaf = is.leaf(n)
        attr(n, "node_ix") = tmp.env2345089712349876$i
        tmp.env2345089712349876$i = tmp.env2345089712349876$i + 1
        num_branches = length(elementNROWS(n))
        midpoint = attr(n, "midpoint")
        if (is.null(midpoint)) {
            midpoint = NA
        }
        value = attr(n, "value")
        if (is.null(value)) {
            value = NA
        }
        if (is_leaf) {
            lb = labels(n)
        } else {
            lb = NA
        }
        tmp.env2345089712349876$lst = c(tmp.env2345089712349876$lst, list(data.frame(height = ht, members = mem, is_leaf = is_leaf, num_branches = num_branches, node_ix  = attr(n, "node_ix"), value = value, midpoint = midpoint, label = lb)))
        return(n)
    })
    dendrapply(new_dend, function(n) {
        parent_ix = attr(n, "node_ix")
        child_ix = sapply(n, function(x) attr(x, "node_ix"))
        if (!is.leaf(n)) {
            tmp.env2345089712349876$lst2 = c(tmp.env2345089712349876$lst2, list(data.frame(parent_ix = parent_ix, child_ix = child_ix)))
        }
        return(NULL)
    })
    these_dt = rbindlist(tmp.env2345089712349876$lst)
    p_c_tbl = rbindlist(tmp.env2345089712349876$lst2)
    ## these_dt[, node_id := 1:.N]
    these_dt = merge(these_dt, p_c_tbl, by.x = "node_ix", by.y = "parent_ix", all.x = TRUE)
    rm(list = "tmp.env2345089712349876")
    return(these_dt)
    ## return(rbindlist(node_tbl))
}


cutoff_clusters = function(t_tbl) {
    these_heights = sort(t_tbl[, unique(height)])
    lst_of_clusters = lapply(these_heights, function(ht) {
        num_clusters = t_tbl[height >= ht, length(unique(child_ix))]
        return(setNames(ht, num_clusters))
    })
    num_clust = unlist(lst_of_clusters)
    these_names = as.character(sort(as.integer(names(num_clust))))
    num_clust = num_clust[these_names]
    names(num_clust) = ceiling(as.integer(these_names) / 2)
    return(num_clust)
}


## helper function for lapply
seq_which = function(expression) {
    if (inherits(expression, "logical")) {
        return(which(expression))
    } else {
        return(seq_along(expression))
    }
}


## rcplex implementation of nnls()
## based on Marcin's signatures solver
rcplex_nnls = function(A, b) {
    orig_A = A
    varnames = c(paste('Col', 1:ncol(A),  sep = "_"),
                 paste0('eps', 1:nrow(A)))
    A = cbind(A, diag(rep(1, nrow(A))))
    colnames(A) = varnames
    sense = 'E'
    lb = rep(-Inf, ncol(A))
    lb[1:ncol(orig_A)] = 0
    ub = rep(Inf, ncol(A))
    Q = diag(c(rep(0, ncol(orig_A)), rep(1, nrow(orig_A)))) ## sum of squares of just epsilons
    sol = Rcplex::Rcplex(cvec = rep(0, length(varnames)), Amat = A, bvec = as.numeric(b), sense = sense, Qmat = Q, lb = lb, ub = ub, objsense = "min", vtype = rep("C", length(varnames)))
    if (!is.null(colnames(orig_A))) {
        col_nm = colnames(orig_A)
    } else {
        col_nm = as.character(1:ncol(orig_A))
    }
    solution_df = data.frame(column_name = col_nm, coefficients = round(sol$xopt[1:ncol(orig_A)]))
    sum_residuals = round(sum(sol$xopt[-(1:ncol(orig_A))]))
    solution_df = rbind(solution_df, data.table(column_name = "Residual", coefficients = sum_residuals))
    return(solution_df)
}


.wclass = function(w, param = c(0, 100, 1e3, 1e4, 1e5, 1e6, 1e7, Inf)) as.character(cut(w, param))
.walks2juncs = function(walks)
{
  if (length(walks)==0)
    return(NULL)
  ix = which(values(walks)$str == '+' & elementNROWS(walks)>1)
  wkdt = as.data.table(walks[ix, ])
  juncs = NULL
  if (nrow(wkdt)>0)
    {
      juncdt = wkdt[, .(
        seg1 = gr.string(GRanges(seqnames[-.N], IRanges(start[-.N], end[-.N]), strand = strand[-.N])),
        b1 = gr.string(gr.flipstrand(gr.end(GRanges(seqnames[-.N], IRanges(start[-.N], end[-.N]), strand = strand[-.N]), 1, ignore.strand = FALSE))),
        b2 = gr.string(gr.start(GRanges(seqnames[-1], IRanges(start[-1], end[-1]), strand = strand[-1]), 1, ignore.strand = FALSE)),
        seg2 = gr.string(GRanges(seqnames[-1], IRanges(start[-1], end[-1]), strand = strand[-1]))),
        by = group]
      juncs = grl.pivot(GRangesList(parse.gr(juncdt$b1), parse.gr(juncdt$b2)))
      values(juncs) = as.data.frame(juncdt[, .(b1, b2, seg1, seg2, walk = ix[group])])
    }
  return(juncs)
}

make_token_tbl = function(gwalk_path, jabba_path, j_class_path = NA, id = NULL, outdir, with_cn = TRUE) {
    library(copynumber)
    source('/gpfs/commons/home/khadi/modules/RA_ClustClassify/utility.R')
    class_levels = c("Clust_del_1_to_10kb","Clust_del_10_to_100kb","Clust_del_100kb_to_1Mb","Clust_del_1Mb_to_10Mb","Clust_del_gr_10Mb","Clust_tds_1_to_10kb","Clust_tds_10_to_100kb","Clust_tds_100kb_to_1Mb","Clust_tds_1Mb_to_10Mb","Clust_tds_gr_10Mb","Clust_inv_1_to_10kb","Clust_inv_10_to_100kb","Clust_inv_100kb_to_1Mb","Clust_inv_1Mb_to_10Mb","Clust_inv_gr_10Mb","Clust_trans","Nonclust_del_1_to_10kb","Nonclust_del_10_to_100kb","Nonclust_del_100kb_to_1Mb","Nonclust_del_1Mb_to_10Mb","Nonclust_del_gr_10Mb","Nonclust_tds_1_to_10kb","Nonclust_tds_10_to_100kb","Nonclust_tds_100kb_to_1Mb","Nonclust_tds_1Mb_to_10Mb","Nonclust_tds_gr_10Mb","Nonclust_inv_1_to_10kb","Nonclust_inv_10_to_100kb","Nonclust_inv_100kb_to_1Mb","Nonclust_inv_1Mb_to_10Mb","Nonclust_inv_gr_10Mb","Nonclust_trans")
    arg_lengths = length(gwalk_path) == length(jabba_path)
    if (!arg_lengths) {
        stop("not every gwalk has a corresponding jabba path")
    }
    if (is.null(id)) {
        id = 1:length(gwalk_path)
    } else {
        if (length(id) != length(gwalk_path)) {
            stop("ids must have one to one correspondence to gwalk_path and jabba_path")
        }
    }
    out_tbl = mcMap(function(gp, jp, jclassp, this_id) {
        tryCatch({
            message('Starting ', this_id)
            ## jab = readRDS(pairs[this.pair,]$jabba_rds)
            jab = readRDS(jp)
            junctions = jab$junctions
            if (any(unlist(width(junctions) != 2))) {
                ## width(junctions) = endoapply(width(junctions), function(x) as.integer(ifelse(x != 2, 2, x)))
                junctions = grl.start(junctions, width = 2, force = TRUE, clip = FALSE)
                ## junctions = endoapply(junctions, function(x) x + 1)
            }
            wann = NULL
            if (any(values(jab$junctions)$cn>0))
            {
                ## walks = muffle(readRDS(pairs[this.pair,]$gwalk))
                walks = readRDS(gp)
                walks = walks[! unlist(lapply(walks, function(w)  any(! seqlevelsInUse(w) %in% c(1:22, "X", "Y", "M", "MT"))))]
                wj = .walks2juncs(walks)
                if (length(wj)>0)
                {
                    values(wj)$w1 = width(parse.gr(values(wj)$seg1))
                    values(wj)$w2 = width(parse.gr(values(wj)$seg2))
                    values(wj)$ab.id = as.data.table(ra.overlaps(wj, junctions, pad = 2, maxgap = -1L, minoverlap = 0L))[, ra2.ix[1], keyby = ra1.ix][.(1:length(wj)), V1]
                    ## jclass = fread(pairs[this.pair, junc_sig_prob])[, ra_class[1], keyby = grl.ix]
                    if (is.na(jclassp)) {
                        jclass = ra_sig_classify(junctions, class_levels)[[1]][, ra_class[1], keyby = grl.ix]                        
                    } else {
                        jclass = fread(jclassp)
                    }
                    values(wj)$cn = values(junctions)[values(wj)$ab.id, "cn"]
                    ## return(data.table(pair = this.pair, ab.id = values(wj)$ab.id, cn = values(wj)$cn)) # used to get a sense of junction copy number across all CCLE
                    #' collapsing all copy number greater than 3
                    cn_lab = sprintf("cn%s", factor(as.character(pmin(values(wj)$cn, 3)), levels = c("1", "2", "3"),  labels = c("1", "2", "3+")))
                    jclass_cn = paste0(jclass[.(values(wj)$ab.id), V1], "_", cn_lab)
                    values(wj)$jclass = jclass[.(values(wj)$ab.id), V1]
                    values(wj)$iclass1 =  .wclass(values(wj)$w1)
                    values(wj)$iclass2 = .wclass(values(wj)$w2)
                    values(wj)$jclass_cn = jclass_cn
                    values(wj)$cn_lab = cn_lab
                    if (!with_cn) {
                        wann = as.data.table(values(wj))[, .(
                                                token = c(as.vector(rbind(iclass1, jclass)), iclass2[.N]),
                                                type = c(as.vector(rbind(rep('interval', .N), rep('junction',.N))), 'interval')
                                            ), keyby = walk]
                    } else {
                        wann = as.data.table(values(wj))[, .(
                                                ## token = c(as.vector(rbind(iclass1, jclass)), iclass2[.N]),
                                                token = c(as.vector(rbind(iclass1, jclass, cn_lab)), iclass2[.N]),
                                                type = c(as.vector(rbind(rep('interval', .N), rep('junction',.N), rep('junc_cn',.N))), 'interval')
                                            ), keyby = walk]
                        remove_these = wann[type == "junc_cn"][!token %in% c("cn1", "cn2", "cn3+")][["walk"]]
                        wann = wann[! walk %in% remove_these]
                    }
                    wann$pair = this_id
                }
            }
            outpath = rm_mparen(paste0(normalizePath(outdir), "/", this_id, ".rds"))
            saveRDS(wann, outpath)
            message('Dumped ', this_id)
            return(data.table(id = this_id, status = "fine", path = outpath))
        }, error = function(e) data.table(id = this_id, status = "error", path = NA))
    }, gwalk_path, jabba_path, j_class_path, id)
    return(rbindlist(out_tbl))
}


write_seq_to_fasta = function(sequences, alphabet  = NULL, outdir, alphabet_fn = NULL, sequence_fn = NULL) {
    if (is.null(sequence_fn)) {
        sequence_fn = "/sequences.fa"
    }
    if (is.null(alphabet_fn)) {
        alphabet_fn = "/alphabet.file"
    }
    if (is.null(alphabet)) {
        if (is.character(sequences))
        sequences = DNAStringSet(sequences)
        if (is.null(names(sequences))) 
            names(sequences) = as.character(1:length(sequences))
        writeXStringSet(sequences, rm_mparen(paste0(outdir, "/", sequence_fn)))
    } else {
        alphabet.path = rm_mparen(paste0(outdir, "/", alphabet_fn))
        writeLines(c("ALPHABET custom", alphabet), alphabet.path)
        if (is.null(names(sequences))) 
            names(sequences) = 1:length(sequences)
        writeLines(as.vector(rbind(paste0(">", names(sequences)), 
                                   sequences)), rm_mparen(paste0(outdir, "/", sequence_fn)))
    }
}




make_indel_synteny = function(vars, ref_field, alt_field, make_gchain = TRUE, make_list = !make_gchain) {
    is_indel = nchar(gv(vars, ref_field)) - nchar(gv(vars, alt_field)) != 0
    vars = sort(vars[is_indel])
    vars = vars[, c(ref_field, alt_field)]

    if (!inherits(gv(vars, ref_field), "character")) {
        if (inherits(gv(vars, ref_field), "DNAStringSet")) {
            mcols(vars)[, ref_field] = as.character(gv(vars, ref_field))
        } else {
            stop("ref_field must be character or DNAStringSet")
        }
    }

    if (!inherits(gv(vars, alt_field), "character")) {
        if (inherits(gv(vars, alt_field), "DNAStringSetList")) {
            alt_split = S4Vectors::unstrsplit(gv(vars, alt_field), sep = ",")

            ## just take 1st haplotype for now
            char_alt = sub("^([A-Z]*)(\\,)([A-Z]*)", "\\1", alt_split)

            ## ## for 2nd haplotype
            ## alt_hi_2 = sub("^([A-Z]*)(\\,)([A-Z]*)", "\\3", alt_hi_split)

            mcols(vars)[, alt_field] = as.character(char_alt)
            rm(list = c('char_alt', "alt_split"))
        } else {
            stop("alt_field must be character or DNAStringSetList")
        }
    }
    
    mcols(vars)[nchar(gv(vars, alt_field)) - nchar(gv(vars, ref_field)) > 0,"type"] = "INS"
    mcols(vars)[nchar(gv(vars, alt_field)) - nchar(gv(vars, ref_field)) < 0,"type"] = "DEL"

    gp = gaps(vars) %Q% (strand == "*")
    full_g = sort(grbind(gp, vars))
    ## mcols(full_g)[["delta_len"]] = nchar(gv(full_g, "haplotype_1")) - nchar(gv(full_g, "REF"))
    mcols(full_g)[["delta_len"]] = nchar(gv(full_g, alt_field)) - nchar(gv(full_g, ref_field))
    mcols(full_g)[is.na(gv(full_g, ref_field)),][["delta_len"]] = 0
    dt = gr2dt(full_g)

    ## dt[, REF := ifelse(is.na(REF), "", REF)]
    ## dt[, haplotype_1 := ifelse(is.na(haplotype_1), "", haplotype_1)]
    ## dt[, shift_by := pmax(c(0, head(nchar(ALT) - nchar(REF), -1)), 0), by = seqnames]
    set(dt, j = ref_field, value = ifelse(is.na(dt[[ref_field]]), "", dt[[ref_field]]))
    set(dt, j = alt_field, value = ifelse(is.na(dt[[alt_field]]), "", dt[[alt_field]]))
    ## dt[, shift_by := pmax(c(0, head(nchar(eval(parse(text = alt_field))) - nchar(eval(parse(text = ref_field))), -1)), 0), by = seqnames]
    dt[, shift_by := c(0, head(nchar(eval(parse(text = alt_field))) - nchar(eval(parse(text = ref_field))), -1)), by = seqnames]
    dt[, cum_shift := cumsum(shift_by), by = seqnames]
    ## dt[, cum_shift := cumsum(delta_len), by = seqnames]
    wid_c_dt = dt[, list(wid_change = sum(delta_len)), by = seqnames]
    wid_change = wid_c_dt[, setNames(wid_change, seqnames)]
    seqlevels(new_gr, pruning.mode = "coarse") = seqlevels(vars)
    seqlengths(new_gr) = seqlengths(vars)
    if (is.null(seqlengths(vars))) {
        new_gr = dt2gr(dt)
    } else {
        new_gr = dt2gr(dt, seqlengths = seqlengths(vars))
    }
    ## synt_gr = copy(new_gr)

    ## reduce length of deletion granges
    ix = na2false(new_gr$type == "DEL")
    new_gr[ix] = gr.width(new_gr[ix], width(new_gr[ix]) + mcols(new_gr[ix])[["delta_len"]])
    ## new_gr = gr.width(new_gr, width(new_gr) + mcols(new_gr)[["delta_len"]])
    
    ## new_gr = GenomicRanges::shift(new_gr, mcols(new_gr)[["cum_shift"]])

    synt_gr = new_gr
    

    ## synt_gr = gr.width(synt_gr, width(synt_gr) + mcols(synt_gr)[["delta_len"]])
    synt_gr = GenomicRanges::shift(synt_gr, mcols(synt_gr)[["cum_shift"]])

    ## count up bases in new coordinates
    ## this_grl = split(new_gr, seqnames(new_gr))
    
    ## synt_grl = GRangesList(ret_no_err(lapply(seq_along(this_grl), function(i) {
    ##     try({
    ##         browser()
    ##         g = this_grl[[i]]
    ##         new_ends = cumsum(width(g))
    ##         new_starts = c(1, head(new_ends+1, -1))
    ##         return(GRanges(seqnames(g), IRanges(new_starts, new_ends)))
    ##     }, silent = TRUE)
    ## })))

    ## synt_gr = unlist(synt_grl)
    ## mcols(synt_gr) = mcols(new_gr)
    seqlevels(synt_gr, pruning.mode = "coarse") = seqlevels(vars)
    seqlengths(synt_gr) = seqlengths(vars) + wid_change[names(seqlengths(vars))]

    ## shift to account for insertions
    ## synt_gr = GenomicRanges::shift(synt_gr, synt_gr$shift_by)
    ## synt_gr = GenomicRanges::shift(synt_gr, mcols(synt_gr)[["cum_shift"]])


    if(make_gchain) {
        return(gChain(new_gr, synt_gr))
    } else if (make_list) {
        return(list(orig_coord_gr = new_gr, synt_coord_gr = synt_gr))
    }
}


### functionality to add onto gr2seq
#' browser() within gr2seq()
#' see ~/Projects/Simulation/sim_utils.R


refalt2char = function(vars, ref_field, alt_field)
{
    if (!inherits(gv(vars, ref_field), "character")) {
        if (inherits(gv(vars, ref_field), "DNAStringSet")) {
            mcols(vars)[, ref_field] = as.character(gv(vars, ref_field))
        } else {
            stop("ref_field must be character or DNAStringSet")
        }
    }

    if (!inherits(gv(vars, alt_field), "character")) {
        if (inherits(gv(vars, alt_field), "DNAStringSetList")) {
            alt_split = S4Vectors::unstrsplit(gv(vars, alt_field), sep = ",")

            ## just take 1st haplotype for now
            char_alt = sub("^([A-Z]*)(\\,)([A-Z]*)", "\\1", alt_split)

            ## ## for 2nd haplotype
            ## alt_hi_2 = sub("^([A-Z]*)(\\,)([A-Z]*)", "\\3", alt_hi_split)

            mcols(vars)[, alt_field] = as.character(char_alt)
            rm(list = c('char_alt', "alt_split"))
        } else {
            stop("alt_field must be character or DNAStringSetList")
        }
    }
    return(vars)
}

## all.equal.mult <- function(..., ignore.null = FALSE) {

##     names <- as.character(substitute(list(...)))[-1L]
##     if (ignore.null) {
##         idx = !grepl("^NULL$", names)
##         names = names[idx]
##     } else {
##         idx = 1:length(list(...))
##     }
##                                         # more than one object required
##     if (length(list(...)[idx]) < 2) stop("More than one object required")

##                                         # character vector of object names
##     ## names <- as.character(substitute(list(...)))[-1L]

##     if (length(names) == 1 & is.list(eval(parse(text = names[1])))) {
##         lst = eval(parse(text = names[1]))
##         names(lst) = 1:length(lst)
##     } else {
##         lst = list(...)
##     } ## for list capability -- will figure out later

    
    
##                                         # matrix of object name pairs
##     pairs <- t(combn(names, 2))

##                                         # if only two objects, return one item list containing all.equal() for them
##     ## if (nrow(pairs) == 1) return(list(all.equal(get(pairs[1,1]), get(pairs[1,2]))))
##     if (nrow(pairs) == 1) return(list(all.equal(eval(parse(text = pairs[1,1])), eval(parse(text = pairs[1,2])))))

##                                         # function: eq.fun()
##                                         # description: applies all.equal() to two quoted names of objects
##                                         # input: two quoted names of objects
##                                         # output: list containing all.equal() comparison and "[obj1] vs. [obj2]"
##                                         # examples:
##                                         #   x <- 1
##                                         #   y <- 1
##                                         #   z <- 2
##                                         #   eq.fun("x", "y") # list(TRUE, "x vs. y")
##                                         #   eq.fun("x", "z") # list("Mean relative difference: 1", "x vs. z")
##     eq.fun <- function(x, y, this_env = parent.frame(2)) {
##         all.eq  = all.equal(eval(parse(text = x), envir = this_env), eval(parse(text = y), envir = this_env))
##         ## all.eq <- all.equal(get(x, inherits=TRUE), get(y, inherits=TRUE))
##         name <- paste0(x, " vs. ", y)
##         return(list(all.eq, name))
##     }

##                                         # list of eq.fun object comparisons
##     out <- vector(mode="list", length=nrow(pairs))

##     for (x in 1:nrow(pairs)) {
##         eq.list <- eq.fun(pairs[x, 1], pairs[x, 2])
##         out[[x]] <- eq.list[[1]]
##         names(out)[x] <- eq.list[[2]]
##     }

##                                         # return TRUE if all objects equal, comparison list otherwise
##     if (mode(unlist(out)) == "logical") {return(TRUE)} else {return(out)}
## }


all.equal.mult <- function(..., ignore.null = FALSE) {

    names <- as.character(substitute(list(...)))[-1L]
    if (ignore.null) {
        idx = !grepl("^NULL$", names)
        names = names[idx]
    } else {
        idx = 1:length(list(...))
    }
                                        # more than one object required
    if (length(list(...)[idx]) < 2) stop("More than one object required")

                                        # character vector of object names
    ## names <- as.character(substitute(list(...)))[-1L]

    if (length(names) == 1 & is.list(eval(parse(text = names[1])))) {
        lst = eval(parse(text = names[1]))
    } else {
        lst = list(...)
        names(lst) = names
    } ## for list capability -- will figure out later

    
    
                                        # matrix of object name pairs
    pairs = t(combn(1:length(lst), 2))
    

                                        # if only two objects, return one item list containing all.equal() for them
    if (nrow(pairs) == 1) return(list(all.equal(lst[[pairs[1,1]]], lst[[pairs[1,2]]])))

                                        # function: eq.fun()
                                        # description: applies all.equal() to two quoted names of objects
                                        # input: two quoted names of objects
                                        # output: list containing all.equal() comparison and "[obj1] vs. [obj2]"
                                        # examples:
                                        #   x <- 1
                                        #   y <- 1
                                        #   z <- 2
                                        #   eq.fun("x", "y") # list(TRUE, "x vs. y")
                                        #   eq.fun("x", "z") # list("Mean relative difference: 1", "x vs. z")
    eq.fun <- function(x, y, this_env = parent.frame(2)) {
        all.eq  = all.equal(lst[[x]], lst[[y]])
        ## all.eq <- all.equal(get(x, inherits=TRUE), get(y, inherits=TRUE))
        name <- paste0(x, " vs. ", y)
        return(list(all.eq, name))
    }

                                        # list of eq.fun object comparisons
    out <- vector(mode="list", length=nrow(pairs))

    for (x in 1:nrow(pairs)) {
        eq.list <- eq.fun(pairs[x, 1], pairs[x, 2])
        out[[x]] <- eq.list[[1]]
        names(out)[x] <- eq.list[[2]]
    }

                                        # return TRUE if all objects equal, comparison list otherwise
    if (mode(unlist(out)) == "logical") {return(TRUE)} else {return(out)}
}

hmap2_breaks = function(inputmat, mid = 0, n_breaks = 100, palette = "RdYlBu", n_col = 11, rev_col = FALSE)
{
    if (n_col %% 2 == 0) {
        message('n_col should be an odd number')
        n_col = n_col + 1
    }
    breaks <- seq(from=min(range(inputmat)), to=max(range(inputmat)), length.out=n_breaks)
    midpoint <- which.min(abs(breaks - mid))
    these_cols = brewer.master(n_col, palette)
    if (rev_col) {
        these_cols = rev(these_cols)
    }
    col1 = these_cols[1:ceiling(length(these_cols)/2)]
    col2 = these_cols[(ceiling(length(these_cols)/2)):length(these_cols)]
    rampCol1 <- colorRampPalette(col1)(midpoint)
    rampCol2 <- colorRampPalette(col2)(n_breaks-(midpoint+1))
    rampCols <- c(rampCol1,rampCol2)
    return(list(breaks = breaks, col = rampCols))
}
