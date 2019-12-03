ggvpc_xpose =
   function (vpc,
             PI = c(0.025, 0.975),
             area.col.central = PI.ci.med.arcol,
             area.col.outer = gray(0.2),
             linecol.pred = PI.real.med.col,
             linetype.obs.central = 'solid',
             linetype.obs.outer = 'dashed',
             linecol.obs.central = PI.real.med.col,
             linecol.obs.outer = 'darkslategrey',
             linesize.obs = 0.5,
             area.alpha = 0.33,
             point.shape = 1,
             point.size = 1.25,
             point.col = 'darkslategrey',
             point.alpha = 0.5,
             yrange.stretch = c(0.9, 1.1),
             quiet = TRUE)
   {
      if (!quiet)
         message('preparing plot ...')
      names(vpc$obs)[names(vpc$obs) == unique(vpc$obs$idv.var)] = 'IVAR'
      names(vpc$obs)[names(vpc$obs) == unique(vpc$obs$dv.var)] = 'DVVAR'
      PI = PI * 100
      cin = names(vpc$vpc)[grepl('CI.for', names(vpc$vpc))][1]
      cin = sub('vpc', '', unPaste(cin, sep = '[.]')[[1]])
      vpc$res$piLowerDown = vpc$res[, paste0('vpc', cin, '.CI.for.',
                                             PI[1], '.from')]
      vpc$res$piLowerUp = vpc$res[, paste0('vpc', cin, '.CI.for.',
                                           PI[1], '.to')]
      vpc$res$piUpperDown = vpc$res[, paste0('vpc', cin, '.CI.for.',
                                             PI[2], '.from')]
      vpc$res$piUpperUp = vpc$res[, paste0('vpc', cin, '.CI.for.',
                                           PI[2], '.to')]
      vpc$res$piCentralDown = vpc$res[, paste0('vpc', cin, '.CI.for.50.from')]
      vpc$res$piCentralUp = vpc$res[, paste0('vpc', cin, '.CI.for.50.to')]
      vpc$vpc$obsLower = vpc$vpc[, paste0('vpc', PI[1], '.real')]
      vpc$vpc$obsUpper = vpc$vpc[, paste0('vpc', PI[2], '.real')]
      p = ggplot() +
         theme_bw() +
         geom_ribbon(
            data = vpc$res,
            aes(x = xCov, ymin = piLowerDown, ymax = piLowerUp)
            ,
            fill = area.col.outer,
            alpha = area.alpha
         ) +
         geom_ribbon(
            data = vpc$res,
            aes(x = xCov, ymin = piUpperDown, ymax = piUpperUp)
            ,
            fill = area.col.outer,
            alpha = area.alpha
         ) +
         geom_ribbon(
            data = vpc$res,
            aes(x = xCov, ymin = piCentralDown, ymax = piCentralUp)
            ,
            fill = area.col.central,
            alpha = area.alpha
         ) +
         geom_line(
            data = vpc$vpc,
            aes(x = xCovm, y = obsLower)
            ,
            linetype = linetype.obs.outer,
            size = linesize.obs
         ) +
         geom_line(
            data = vpc$vpc,
            aes(x = xCovm, y = vpc50.real),
            col = linecol.obs.central,
            linetype = linetype.obs.central,
            size = linesize.obs
         ) +
         geom_line(
            data = vpc$vpc,
            aes(x = xCovm, y = obsUpper)
            ,
            linetype = linetype.obs.outer,
            size = linesize.obs
         ) +
         coord_cartesian(ylim = yrange.stretch * range(vpc$obs$DV)) +
         geom_point(
            data = vpc$obs,
            aes(x = IVAR, y = DVVAR),
            col = point.col,
            shape = point.shape,
            size = point.size,
            alpha = point.alpha
         )
      p
   }