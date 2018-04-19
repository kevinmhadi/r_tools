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
    flip_x = TRUE)
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

        if (scatter)
        {
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

        if (!is.null(dat$facet1))
            {
                if (!is.null(dat$facet2))
                    {
                        if (transpose)
                            g = g + facet_grid(facet2 ~ facet1)
                        else
                            g = g + facet_grid(facet1 ~ facet2)
                    }
                else
                    {
                        if (transpose)
                            g = g + facet_grid(. ~ facet1)
                        else
                            g = g + facet_grid(facet1 ~ .)
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
setColnames = function(object = nm, nm) {
    colnames(object)  = nm
    object
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
    obj = obj[which(!sapply(obj, is.null))]
    if (is.null(names(obj))) {
        nm = sapply(substitute(list(...))[-1], deparse)
        names(obj) = nm
    }
    if (is.null(FUN) & !is.null(expr)) {
        obj = lapply(obj, function(.OBJ) {
            eval(expr)
            return(.OBJ)
        })
    } else if (!is.null(FUN)) {
        obj = lapply(obj, FUN)
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
    gr = unlist(grl)
    gr = newval(gr, cols, val)
    grl = relist(gr, grl)
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
